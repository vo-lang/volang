use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

use vo_common_core::extern_key::decode_extern_name;
use vo_runtime::bytecode::{ExternDef, ExternEffects};
use vo_runtime::ffi::{
    unique_extern_providers, ExternCallContext, ExternContractError, ExternFn, ExternRegistry,
    ExternResult, HostEventReplaySource, VoSlice, VoStringElem,
};
use vo_runtime::gc::{GcLease, GcRef};
use vo_runtime::objects::{closure, string};
use vo_ui_artifact::{
    decode_component_artifact, decode_component_bundle, ArtifactLimits, BindingDefinition,
    BundleLimits, ComponentCallMode, ComponentCallSiteId, ComponentDefinition, ComponentTypeId,
    ExecutionMode, HandlerSiteId, StateFieldId, StateValueKind, COMPONENT_ARTIFACT_NAME,
    COMPONENT_ARTIFACT_VERSION, COMPONENT_BUNDLE_ARTIFACT_NAME, COMPONENT_BUNDLE_ARTIFACT_VERSION,
};
pub use vo_ui_artifact::{ComponentArtifact, ComponentBundle};
use vo_ui_core::{
    EventPayload, EventType, HandlerId, Key, Length, Listener, ListenerOptions, NodeId, Primitive,
    PropertyId, UiEvent, Value, View,
};
use vo_ui_headless::HeadlessRenderer;
use vo_ui_plan::{DirectMutation, PlanLimits, SlotId, SlotKind, SlotValue};
use vo_ui_protocol::{decode_event, encode_batch, ProtocolLimits};
use vo_ui_reload::{plan_reload, ComponentSchema, ReloadLimits, StateAction, StateField};
use vo_ui_runtime::{
    project_template_slots, ComponentHandlerId, ComponentSpec, ComponentStateCell,
    ComponentTemplateRuntime, ComponentValue, TemplateRuntime, UiRuntime,
};
use vo_ui_system::{
    decode_system_response, encode_system_request, ClipboardContent, ClipboardFormat,
    ClipboardImage, DragDropPhase, FileDialogFilter, FileDialogKind, FileDialogRequest,
    FileDragMode, FileDragRequest, HostInvocation, MenuItemId, MenuModel, MenuNode,
    MessageDialogButtons, MessageDialogLevel, MessageDialogRequest, SystemEvent, SystemFailure,
    SystemLimits, SystemRequest, SystemRequestEnvelope, SystemResponse, SystemResponseEnvelope,
};

pub const UI_MODULE_PATH: &str = "github.com/vo-lang/ui";
pub const UI_SYSTEM_MODULE_PATH: &str = "github.com/vo-lang/ui/system";
const MAX_BUILD_VIEWS: usize = 100_000;
const MAX_BUILD_HANDLERS: usize = 65_536;
const MAX_STATE_CELLS: usize = 65_536;
const MAX_NAVIGATION_REQUESTS: usize = 1_024;
const MAX_LOCATION_BYTES: usize = 16 * 1_024;
const MAX_KEY_BYTES: usize = 4 * 1_024;
const MAX_SYSTEM_REQUESTS: usize = 1_024;
const MAX_SYSTEM_REQUEST_BYTES: usize = 32 * 1_024 * 1_024;
const SCOPED_STATE_HANDLE_TAG: u64 = 1_u64 << 63;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PendingSystemRequest {
    pub request_id: u64,
    pub frame: Vec<u8>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NavigationRequest {
    Push(String),
    Replace(String),
    Back,
    Forward,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ViewHandle {
    index: u32,
    generation: u32,
}

impl ViewHandle {
    fn encode(self) -> u64 {
        (u64::from(self.generation) << 32) | u64::from(self.index.saturating_add(1))
    }

    fn decode(value: u64) -> Option<Self> {
        let encoded_index = value as u32;
        let generation = (value >> 32) as u32;
        (encoded_index != 0 && generation != 0).then_some(Self {
            index: encoded_index - 1,
            generation,
        })
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
enum MountPhase {
    #[default]
    Idle,
    AwaitingRoot {
        initial: bool,
    },
    WaitingEvent {
        token: u64,
    },
    AwaitingHandler,
    EvaluatingSlots,
    EvaluatingBundle,
    BundleHandlerReady,
    AwaitingBundleHandler,
    AwaitingDirectCommit,
}

struct DirectUpdate {
    bindings: Vec<usize>,
    cursor: usize,
    awaiting: Option<usize>,
    updates: Vec<(SlotId, SlotValue)>,
}

struct BundleEvalRequest {
    function: u32,
    arguments: Vec<u64>,
    state: Option<(BundleStateKey, StateValueKind)>,
    handler: Option<BundleHandlerKey>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct BundleStateKey {
    component: ComponentTypeId,
    path: Vec<BundleInstanceIdentity>,
    field: u32,
    type_fingerprint: u64,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum BundleInstanceIdentity {
    CallSite(u64),
    Key(Key),
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum GenericComponentIdentity {
    CallSite { id: u64, occurrence: u32 },
    Key(Key),
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct GenericComponentPathSegment {
    component: String,
    instance: GenericComponentIdentity,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct GenericStateKey {
    path: Vec<GenericComponentPathSegment>,
    field: u32,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct GenericOccurrenceKey {
    parent: Vec<GenericComponentPathSegment>,
    component: String,
    call_site: u64,
}

#[derive(Clone, Debug)]
struct GenericComponentFrame {
    path: Vec<GenericComponentPathSegment>,
    state_cursor: u32,
}

#[derive(Clone, Debug)]
struct GenericStateSlot {
    generation: u32,
    cell: Option<StateCell>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct BundleHandlerKey {
    path: Vec<u64>,
    site: HandlerSiteId,
}

#[derive(Clone, Copy)]
enum BundleEvalTarget {
    State(usize),
    Binding(usize),
    Handler(usize),
}

struct BundleEvalFrame {
    type_id: ComponentTypeId,
    call_site: Option<vo_ui_artifact::ComponentCallSiteId>,
    path: Vec<u64>,
    state_path: Vec<BundleInstanceIdentity>,
    key: Option<Key>,
    props: Vec<Vec<u64>>,
    states: Vec<Option<Vec<u64>>>,
    state_values: Vec<Option<ComponentValue>>,
    bindings: Vec<Option<Vec<u64>>>,
    slots: Vec<Option<SlotValue>>,
    children: Vec<ComponentSpec>,
    state_cursor: usize,
    binding_cursor: usize,
    handler_cursor: usize,
    call_cursor: usize,
}

struct BundleRootDraft {
    props: Vec<ComponentValue>,
    state: Vec<ComponentStateCell>,
    slots: Vec<SlotValue>,
    children: Vec<ComponentSpec>,
}

struct BundleEvaluation {
    bundle: ComponentBundle,
    initial: bool,
    frames: Vec<BundleEvalFrame>,
    awaiting: Option<BundleEvalTarget>,
    root: Option<BundleRootDraft>,
}

impl BundleEvaluation {
    fn new(bundle: ComponentBundle, initial: bool) -> Result<Self, String> {
        if !bundle_direct_supported(&bundle) {
            return Err("component bundle requires generic runtime features".to_string());
        }
        let root = bundle.root.clone();
        let mut evaluation = Self {
            bundle,
            initial,
            frames: Vec::new(),
            awaiting: None,
            root: None,
        };
        evaluation.push_frame(root, None, Vec::new(), Vec::new(), None, Vec::new())?;
        Ok(evaluation)
    }

    fn advance(
        &mut self,
        completed: Option<Vec<u64>>,
    ) -> Result<Option<BundleEvalRequest>, String> {
        match (self.awaiting.take(), completed) {
            (Some(target), Some(value)) => self.accept(target, value)?,
            (Some(_), None) => {
                return Err("component evaluator replay contained no result".to_string());
            }
            (None, Some(_)) => {
                return Err("component evaluator replay returned an unexpected result".to_string());
            }
            (None, None) => {}
        }
        loop {
            let Some(frame_index) = self.frames.len().checked_sub(1) else {
                return Ok(None);
            };
            let type_id = self.frames[frame_index].type_id.clone();
            let definition = self.definition(&type_id)?.clone();

            if self.frames[frame_index].state_cursor < definition.states.len() {
                let index = self.frames[frame_index].state_cursor;
                self.frames[frame_index].state_cursor += 1;
                let state = &definition.states[index];
                if let Some(function) = state.initializer_func {
                    let arguments = self.arguments(
                        frame_index,
                        &state.initializer_dependencies,
                        &state.initializer_props,
                    )?;
                    self.awaiting = Some(BundleEvalTarget::State(index));
                    return Ok(Some(BundleEvalRequest {
                        function,
                        arguments,
                        state: Some((
                            BundleStateKey {
                                component: self.frames[frame_index].type_id.clone(),
                                path: self.frames[frame_index].state_path.clone(),
                                field: state.id.value(),
                                type_fingerprint: state.type_fingerprint,
                            },
                            state.value_kind,
                        )),
                        handler: None,
                    }));
                }
                self.frames[frame_index].states[index] = Some(Vec::new());
                continue;
            }

            if self.frames[frame_index].binding_cursor < definition.bindings.len() {
                let index = self.frames[frame_index].binding_cursor;
                self.frames[frame_index].binding_cursor += 1;
                let binding = &definition.bindings[index];
                let function = binding.evaluator_func.ok_or_else(|| {
                    format!("component binding {} has no evaluator", binding.id.value())
                })?;
                let arguments = self.arguments(
                    frame_index,
                    &binding.dependencies,
                    &binding.prop_dependencies,
                )?;
                self.awaiting = Some(BundleEvalTarget::Binding(index));
                return Ok(Some(BundleEvalRequest {
                    function,
                    arguments,
                    state: None,
                    handler: None,
                }));
            }

            if self.frames[frame_index].handler_cursor < definition.handlers.len() {
                let index = self.frames[frame_index].handler_cursor;
                self.frames[frame_index].handler_cursor += 1;
                let handler = &definition.handlers[index];
                let function = handler.evaluator_func.ok_or_else(|| {
                    format!("component handler {} has no evaluator", handler.id.value())
                })?;
                let arguments = self.arguments(
                    frame_index,
                    &handler.captured_state,
                    &handler.captured_props,
                )?;
                self.awaiting = Some(BundleEvalTarget::Handler(index));
                return Ok(Some(BundleEvalRequest {
                    function,
                    arguments,
                    state: None,
                    handler: Some(BundleHandlerKey {
                        path: self.frames[frame_index].path.clone(),
                        site: handler.id,
                    }),
                }));
            }

            if self.frames[frame_index].call_cursor < definition.call_sites.len() {
                let index = self.frames[frame_index].call_cursor;
                self.frames[frame_index].call_cursor += 1;
                let call = &definition.call_sites[index];
                let callee = call
                    .callee
                    .clone()
                    .ok_or_else(|| "direct component call has no static callee".to_string())?;
                let props = call
                    .props_bindings
                    .iter()
                    .map(|binding| {
                        let position = definition
                            .bindings
                            .binary_search_by_key(binding, |candidate| candidate.id)
                            .map_err(|_| "component prop binding is missing".to_string())?;
                        self.frames[frame_index].bindings[position]
                            .clone()
                            .ok_or_else(|| "component prop binding was not evaluated".to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let key = call
                    .key_binding
                    .map(|binding| {
                        let position = definition
                            .bindings
                            .binary_search_by_key(&binding, |candidate| candidate.id)
                            .map_err(|_| "component key binding is missing".to_string())?;
                        let value = self.frames[frame_index].bindings[position]
                            .as_ref()
                            .ok_or_else(|| "component key binding was not evaluated".to_string())?;
                        let [raw] = value.as_slice() else {
                            return Err("component key binding must return one string".to_string());
                        };
                        Ok(Key::from(direct_string(*raw)?))
                    })
                    .transpose()?;
                let mut path = self.frames[frame_index].path.clone();
                path.push(call.id.value());
                let mut state_path = self.frames[frame_index].state_path.clone();
                state_path.push(match key.clone() {
                    Some(key) => BundleInstanceIdentity::Key(key),
                    None => BundleInstanceIdentity::CallSite(call.id.value()),
                });
                self.push_frame(callee, Some(call.id), path, state_path, key, props)?;
                continue;
            }

            let frame = self.frames.pop().expect("component frame exists");
            let state = definition
                .states
                .iter()
                .zip(frame.state_values)
                .map(|(field, value)| {
                    Ok(ComponentStateCell::new(
                        field.id,
                        field.key.clone(),
                        field.type_fingerprint,
                        value.ok_or_else(|| {
                            format!(
                                "component state {} has no published value",
                                field.id.value()
                            )
                        })?,
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?;
            let slots = frame
                .slots
                .into_iter()
                .enumerate()
                .map(|(index, value)| {
                    value.ok_or_else(|| format!("component slot {index} was not evaluated"))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let props = alloc_component_values(frame.props.len());
            if let Some(call_site) = frame.call_site {
                self.frames
                    .last_mut()
                    .ok_or_else(|| "nested component lost its parent frame".to_string())?
                    .children
                    .push({
                        let mut spec = ComponentSpec::new(call_site, frame.type_id)
                            .props(props)
                            .state(state)
                            .slots(slots)
                            .children(frame.children);
                        if let Some(key) = frame.key {
                            spec = spec.keyed(key);
                        }
                        spec
                    });
            } else {
                self.root = Some(BundleRootDraft {
                    props,
                    state,
                    slots,
                    children: frame.children,
                });
                return Ok(None);
            }
        }
    }

    fn accept(&mut self, target: BundleEvalTarget, value: Vec<u64>) -> Result<(), String> {
        let frame = self
            .frames
            .last_mut()
            .ok_or_else(|| "component evaluator lost its frame".to_string())?;
        match target {
            BundleEvalTarget::State(index) => {
                if frame.states[index].is_none() {
                    return Err("component state initializer lost its instance handle".to_string());
                }
            }
            BundleEvalTarget::Binding(index) => {
                let definition = self
                    .bundle
                    .definitions
                    .binary_search_by(|definition| definition.type_id.cmp(&frame.type_id))
                    .ok()
                    .map(|position| &self.bundle.definitions[position])
                    .ok_or_else(|| "component evaluator definition is missing".to_string())?;
                let binding = &definition.bindings[index];
                for (slot, slot_value) in bundle_binding_slot_values(definition, binding, &value)? {
                    frame.slots[slot.index() as usize] = Some(slot_value);
                }
                frame.bindings[index] = Some(value);
            }
            BundleEvalTarget::Handler(index) => {
                if value.len() != 1 {
                    return Err(format!(
                        "component handler {index} returned {} slots; expected one closure",
                        value.len()
                    ));
                }
            }
        }
        Ok(())
    }

    fn arguments(
        &self,
        frame: usize,
        state_dependencies: &[StateFieldId],
        prop_dependencies: &[u16],
    ) -> Result<Vec<u64>, String> {
        let current = &self.frames[frame];
        let definition = self.definition(&current.type_id)?;
        let mut arguments = Vec::new();
        for dependency in state_dependencies {
            let index = definition
                .states
                .binary_search_by_key(dependency, |state| state.id)
                .map_err(|_| "component state dependency is missing".to_string())?;
            arguments.extend(
                current.states[index]
                    .as_ref()
                    .ok_or_else(|| "component state dependency is not initialized".to_string())?
                    .iter()
                    .copied(),
            );
        }
        for dependency in prop_dependencies {
            arguments.extend(
                current
                    .props
                    .get(usize::from(*dependency))
                    .ok_or_else(|| "component prop dependency is missing".to_string())?
                    .iter()
                    .copied(),
            );
        }
        Ok(arguments)
    }

    fn push_frame(
        &mut self,
        type_id: ComponentTypeId,
        call_site: Option<vo_ui_artifact::ComponentCallSiteId>,
        path: Vec<u64>,
        state_path: Vec<BundleInstanceIdentity>,
        key: Option<Key>,
        props: Vec<Vec<u64>>,
    ) -> Result<(), String> {
        let definition = self.definition(&type_id)?;
        if props.len() != usize::from(definition.interface.props_arity) {
            return Err(format!(
                "component {type_id} received an invalid prop arity"
            ));
        }
        self.frames.push(BundleEvalFrame {
            type_id,
            call_site,
            path,
            state_path,
            key,
            props,
            states: vec![None; definition.states.len()],
            state_values: vec![None; definition.states.len()],
            bindings: vec![None; definition.bindings.len()],
            slots: vec![None; definition.plan.slots().len()],
            children: Vec::new(),
            state_cursor: 0,
            binding_cursor: 0,
            handler_cursor: 0,
            call_cursor: 0,
        });
        Ok(())
    }

    fn bind_reused_state(&mut self, handle: u64, value: ComponentValue) -> Result<(), String> {
        let Some(BundleEvalTarget::State(index)) = self.awaiting.take() else {
            return Err("component state reuse has no pending initializer".to_string());
        };
        let frame = self
            .frames
            .last_mut()
            .ok_or_else(|| "component state reuse lost its frame".to_string())?;
        frame.states[index] = Some(vec![handle]);
        frame.state_values[index] = Some(value);
        Ok(())
    }

    fn bind_new_state(&mut self, handle: u64) -> Result<(), String> {
        let Some(BundleEvalTarget::State(index)) = self.awaiting else {
            return Err("component state allocation has no pending initializer".to_string());
        };
        let frame = self
            .frames
            .last_mut()
            .ok_or_else(|| "component state allocation lost its frame".to_string())?;
        frame.states[index] = Some(vec![handle]);
        Ok(())
    }

    fn publish_new_state_value(&mut self, value: ComponentValue) -> Result<(), String> {
        let Some(BundleEvalTarget::State(index)) = self.awaiting else {
            return Err("component state value has no pending initializer".to_string());
        };
        let frame = self
            .frames
            .last_mut()
            .ok_or_else(|| "component state value lost its frame".to_string())?;
        frame.state_values[index] = Some(value);
        Ok(())
    }

    fn definition(&self, type_id: &ComponentTypeId) -> Result<&ComponentDefinition, String> {
        self.bundle
            .definitions
            .binary_search_by(|definition| definition.type_id.cmp(type_id))
            .ok()
            .map(|position| &self.bundle.definitions[position])
            .ok_or_else(|| format!("component definition {type_id} is missing"))
    }

    fn take_root(&mut self) -> Result<BundleRootDraft, String> {
        self.root
            .take()
            .ok_or_else(|| "component evaluator produced no root".to_string())
    }
}

fn alloc_component_values(len: usize) -> Vec<ComponentValue> {
    core::iter::repeat_n(ComponentValue::Unit, len).collect()
}

fn bundle_direct_supported(bundle: &ComponentBundle) -> bool {
    bundle.definitions.iter().all(|definition| {
        definition.mode == ExecutionMode::Direct
            && definition.effects.is_empty()
            && definition.tasks.is_empty()
            && definition.lifecycle.mounted_func.is_none()
            && definition.lifecycle.updated_func.is_none()
            && definition.lifecycle.disposing_func.is_none()
            && definition.states.iter().all(|state| {
                state.value_kind != StateValueKind::Opaque
                    && state.has_initializer
                    && state.initializer_func.is_some()
            })
            && definition
                .bindings
                .iter()
                .all(|binding| binding.evaluator_func.is_some())
            && definition
                .handlers
                .iter()
                .all(|handler| handler.evaluator_func.is_some())
            && definition
                .call_sites
                .iter()
                .all(|call| call.mode == ComponentCallMode::Static && call.callee.is_some())
    }) && bundle
        .definitions
        .binary_search_by(|definition| definition.type_id.cmp(&bundle.root))
        .ok()
        .is_some_and(|index| bundle.definitions[index].interface.props_arity == 0)
}

fn bundle_binding_slot_values(
    definition: &ComponentDefinition,
    binding: &BindingDefinition,
    result: &[u64],
) -> Result<Vec<(SlotId, SlotValue)>, String> {
    if binding.slots.is_empty() {
        return Ok(Vec::new());
    }
    let [raw] = result else {
        return Err(format!(
            "component binding {} returned {} slots; expected one UI slot value",
            binding.id.value(),
            result.len()
        ));
    };
    let mut values = Vec::with_capacity(binding.slots.len());
    for slot in &binding.slots {
        let value = match definition.plan.slot_kind(*slot) {
            Some(SlotKind::Text) => SlotValue::Text(direct_string(*raw)?),
            Some(SlotKind::Property) => {
                let (target, property) = definition
                    .plan
                    .update_sites(*slot)
                    .into_iter()
                    .flatten()
                    .find_map(|site| match site.mutation {
                        DirectMutation::SetProperty { target, property } => {
                            Some((target, property))
                        }
                        DirectMutation::SetText { .. } => None,
                    })
                    .ok_or_else(|| {
                        format!("component property slot {} has no site", slot.index())
                    })?;
                let primitive = match definition.plan.node(target).kind {
                    vo_ui_plan::TemplateNodeKind::Element(primitive) => Some(primitive),
                    vo_ui_plan::TemplateNodeKind::Text => None,
                };
                SlotValue::Property(direct_property_value(property, primitive, *raw)?)
            }
            None => return Err(format!("component slot {} is invalid", slot.index())),
        };
        values.push((*slot, value));
    }
    Ok(values)
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ReactiveProfile {
    pub changed_state_writes: u64,
    pub root_evaluations: u64,
    pub direct_update_turns: u64,
    pub scheduled_bindings: u64,
    pub evaluator_calls: u64,
    pub submitted_slots: u64,
    pub emitted_revisions: u64,
    pub emitted_mutations: u64,
    pub no_op_updates: u64,
}

impl ReactiveProfile {
    fn record_batch(&mut self, batch: Option<&vo_ui_protocol::MutationBatch>) {
        let Some(batch) = batch else {
            self.no_op_updates = self.no_op_updates.saturating_add(1);
            return;
        };
        self.emitted_revisions = self.emitted_revisions.saturating_add(1);
        self.emitted_mutations = self
            .emitted_mutations
            .saturating_add(batch.mutations.len() as u64);
    }
}

struct BuildArena {
    generation: u32,
    views: Vec<View>,
    state_cells: Vec<StateCell>,
    generic_state_cells: Vec<GenericStateSlot>,
    generic_state_free: Vec<usize>,
    generic_state_handles: BTreeMap<GenericStateKey, usize>,
    generic_state_live: BTreeSet<GenericStateKey>,
    generic_component_stack: Vec<GenericComponentFrame>,
    generic_occurrences: BTreeMap<GenericOccurrenceKey, u32>,
    reload_cells: BTreeMap<usize, StateCell>,
    state_cursor: usize,
    /// Leases keep handler closures alive for the lifetime of the owning VM.
    handlers: Vec<GcLease>,
    phase: MountPhase,
    session_epoch: u64,
    reload_session_epoch: Option<u64>,
    last_event_sequence: u64,
    component: Option<ComponentArtifact>,
    component_bundle: Option<ComponentBundle>,
    bundle_evaluation: Option<BundleEvaluation>,
    bundle_state_handles: BTreeMap<BundleStateKey, u64>,
    bundle_pending_state: Option<(u64, StateValueKind)>,
    bundle_handlers: BTreeMap<ComponentHandlerId, GcLease>,
    bundle_next_handlers: BTreeMap<BundleHandlerKey, GcLease>,
    bundle_pending_handler: Option<BundleHandlerKey>,
    pending_component_event: Option<(ComponentHandlerId, UiEvent)>,
    bundle_state_checkpoint: Option<BundleStateCheckpoint>,
    runtime: Option<MountedRuntime>,
    invalidation_pending: bool,
    dirty_states: BTreeSet<u32>,
    force_direct_slots: bool,
    direct_update: Option<DirectUpdate>,
    direct_batch: Option<Vec<u8>>,
    direct_commit_initial: bool,
    evaluator_leases: Vec<Option<GcLease>>,
    location: String,
    viewport_width: f64,
    viewport_height: f64,
    scale_factor: f64,
    navigation_requests: Vec<NavigationRequest>,
    system_requests: VecDeque<PendingSystemRequest>,
    system_request_bytes: usize,
    profile: ReactiveProfile,
}

#[derive(Clone, Debug, PartialEq)]
enum StateCell {
    String(String),
    Bool(bool),
    Int(i64),
    Float(f64),
}

#[derive(Clone, Debug, PartialEq)]
struct BundleStateCheckpoint {
    cells: Vec<StateCell>,
    handles: BTreeMap<BundleStateKey, u64>,
    dirty: BTreeSet<u32>,
}

impl BundleStateCheckpoint {
    fn capture(arena: &BuildArena) -> Self {
        Self {
            cells: arena.state_cells.clone(),
            handles: arena.bundle_state_handles.clone(),
            dirty: arena.dirty_states.clone(),
        }
    }

    fn restore(self, arena: &mut BuildArena) {
        arena.state_cells = self.cells;
        arena.bundle_state_handles = self.handles;
        arena.dirty_states = self.dirty;
    }
}

enum MountedRuntime {
    Generic(Box<UiRuntime<HeadlessRenderer>>),
    Template(Box<TemplateRuntime<HeadlessRenderer>>),
    Component(Box<ComponentTemplateRuntime<HeadlessRenderer>>),
}

impl MountedRuntime {
    fn renderer(&self) -> &HeadlessRenderer {
        match self {
            Self::Generic(runtime) => runtime.renderer(),
            Self::Template(runtime) => runtime.renderer(),
            Self::Component(runtime) => runtime.renderer(),
        }
    }
}

impl Default for BuildArena {
    fn default() -> Self {
        Self {
            generation: 0,
            views: Vec::new(),
            state_cells: Vec::new(),
            generic_state_cells: Vec::new(),
            generic_state_free: Vec::new(),
            generic_state_handles: BTreeMap::new(),
            generic_state_live: BTreeSet::new(),
            generic_component_stack: Vec::new(),
            generic_occurrences: BTreeMap::new(),
            reload_cells: BTreeMap::new(),
            state_cursor: 0,
            handlers: Vec::new(),
            phase: MountPhase::Idle,
            session_epoch: 0,
            reload_session_epoch: None,
            last_event_sequence: 0,
            component: None,
            component_bundle: None,
            bundle_evaluation: None,
            bundle_state_handles: BTreeMap::new(),
            bundle_pending_state: None,
            bundle_handlers: BTreeMap::new(),
            bundle_next_handlers: BTreeMap::new(),
            bundle_pending_handler: None,
            pending_component_event: None,
            bundle_state_checkpoint: None,
            runtime: None,
            invalidation_pending: false,
            dirty_states: BTreeSet::new(),
            force_direct_slots: false,
            direct_update: None,
            direct_batch: None,
            direct_commit_initial: false,
            evaluator_leases: Vec::new(),
            location: "/".to_string(),
            viewport_width: 1024.0,
            viewport_height: 768.0,
            scale_factor: 1.0,
            navigation_requests: Vec::new(),
            system_requests: VecDeque::new(),
            system_request_bytes: 0,
            profile: ReactiveProfile::default(),
        }
    }
}

impl BuildArena {
    fn reset(&mut self, component: Option<ComponentArtifact>) {
        *self = Self {
            component,
            ..Self::default()
        };
    }

    fn begin_render(&mut self, initial: bool) {
        self.profile.root_evaluations = self.profile.root_evaluations.saturating_add(1);
        self.generation = self.generation.wrapping_add(1).max(1);
        self.views.clear();
        self.state_cursor = 0;
        self.generic_state_live.clear();
        self.generic_component_stack.clear();
        self.generic_occurrences.clear();
        self.phase = MountPhase::AwaitingRoot { initial };
    }

    fn insert(&mut self, view: View) -> Result<u64, &'static str> {
        if self.views.len() >= MAX_BUILD_VIEWS {
            return Err("UI construction exceeded the per-mount View limit");
        }
        let index = u32::try_from(self.views.len())
            .map_err(|_| "UI construction exhausted View identities")?;
        self.views.push(view);
        Ok(ViewHandle {
            index,
            generation: self.generation,
        }
        .encode())
    }

    fn get(&self, encoded: u64) -> Result<&View, &'static str> {
        let handle = ViewHandle::decode(encoded).ok_or("invalid UI View handle")?;
        if handle.generation != self.generation {
            return Err("stale UI View handle");
        }
        self.views
            .get(handle.index as usize)
            .ok_or("missing UI View handle")
    }

    fn add_handler(&mut self, lease: GcLease) -> Result<HandlerId, &'static str> {
        if self.handlers.len() >= MAX_BUILD_HANDLERS {
            return Err("UI construction exceeded the per-mount handler limit");
        }
        let index = u32::try_from(self.handlers.len())
            .map_err(|_| "UI construction exhausted handler identities")?;
        self.handlers.push(lease);
        Ok(HandlerId::new(index, self.handler_generation()))
    }

    fn handler_lease(&self, handler: HandlerId) -> Result<GcLease, &'static str> {
        if handler.generation() != self.handler_generation() {
            return Err("stale UI event handler generation");
        }
        self.handlers
            .get(handler.index() as usize)
            .copied()
            .ok_or("missing UI event handler")
    }

    fn validates_event(&self, event: &UiEvent) -> bool {
        let Some(runtime) = &self.runtime else {
            return false;
        };
        if event.event == EventType::INVALIDATE {
            return event.handler.index() == u32::MAX && event.target == runtime.renderer().root();
        }
        runtime
            .renderer()
            .node(event.target)
            .and_then(|node| node.listeners.get(&event.event).copied())
            .is_some_and(|listener| listener.handler == event.handler)
    }

    fn handler_generation(&self) -> u32 {
        if self.component.is_some() {
            1
        } else {
            self.generation
        }
    }

    fn use_state(&mut self, initial: StateCell) -> Result<u64, &'static str> {
        if !matches!(self.phase, MountPhase::AwaitingRoot { .. }) {
            return Err("UI state declarations may only run while building the mounted root");
        }
        if !self.generic_component_stack.is_empty() {
            return self.use_generic_state(initial);
        }
        let index = self.state_cursor;
        if index >= MAX_STATE_CELLS {
            return Err("UI state declaration limit exceeded");
        }
        self.state_cursor += 1;
        if let Some(existing) = self.state_cells.get(index) {
            if core::mem::discriminant(existing) != core::mem::discriminant(&initial) {
                return Err("UI state declaration order changed its value type");
            }
        } else {
            let state = match self.reload_cells.remove(&index) {
                Some(state)
                    if core::mem::discriminant(&state) == core::mem::discriminant(&initial) =>
                {
                    state
                }
                Some(_) => {
                    return Err("reloaded UI state does not match its declared value type");
                }
                None => initial,
            };
            self.state_cells.push(state);
        }
        u64::try_from(index + 1).map_err(|_| "UI state identity space is exhausted")
    }

    fn state(&self, handle: u64) -> Result<&StateCell, &'static str> {
        if handle & SCOPED_STATE_HANDLE_TAG != 0 {
            let (index, generation) = decode_scoped_state_handle(handle)?;
            return self
                .generic_state_cells
                .get(index)
                .filter(|slot| slot.generation == generation)
                .and_then(|slot| slot.cell.as_ref())
                .ok_or("stale UI component state handle");
        }
        let index = handle
            .checked_sub(1)
            .and_then(|index| usize::try_from(index).ok())
            .ok_or("invalid UI state handle")?;
        self.state_cells.get(index).ok_or("stale UI state handle")
    }

    fn state_mut(&mut self, handle: u64) -> Result<&mut StateCell, &'static str> {
        if handle & SCOPED_STATE_HANDLE_TAG != 0 {
            let (index, generation) = decode_scoped_state_handle(handle)?;
            return self
                .generic_state_cells
                .get_mut(index)
                .filter(|slot| slot.generation == generation)
                .and_then(|slot| slot.cell.as_mut())
                .ok_or("stale UI component state handle");
        }
        let index = handle
            .checked_sub(1)
            .and_then(|index| usize::try_from(index).ok())
            .ok_or("invalid UI state handle")?;
        self.state_cells
            .get_mut(index)
            .ok_or("stale UI state handle")
    }

    fn int_state_committed(&self, handle: u64) -> bool {
        matches!(self.phase, MountPhase::WaitingEvent { .. })
            && matches!(self.state(handle), Ok(StateCell::Int(_)))
    }

    fn finish_state_declarations(&mut self) -> Result<(), &'static str> {
        if !self.generic_component_stack.is_empty() {
            return Err("UI component scope was not exited before root commit");
        }
        self.state_cells.truncate(self.state_cursor);
        let stale = self
            .generic_state_handles
            .keys()
            .filter(|key| !self.generic_state_live.contains(*key))
            .cloned()
            .collect::<Vec<_>>();
        for key in stale {
            if let Some(index) = self.generic_state_handles.remove(&key) {
                if let Some(slot) = self.generic_state_cells.get_mut(index) {
                    slot.cell = None;
                    self.generic_state_free.push(index);
                }
            }
        }
        self.generic_state_live.clear();
        self.generic_occurrences.clear();
        self.reload_cells.clear();
        self.dirty_states.clear();
        self.force_direct_slots = false;
        Ok(())
    }

    fn mark_state_dirty(&mut self, handle: u64) -> Result<(), &'static str> {
        if handle & SCOPED_STATE_HANDLE_TAG != 0 {
            self.state(handle)?;
            self.profile.changed_state_writes = self.profile.changed_state_writes.saturating_add(1);
            return Ok(());
        }
        let index = handle
            .checked_sub(1)
            .and_then(|index| u32::try_from(index).ok())
            .ok_or("invalid UI state handle")?;
        if index as usize >= self.state_cells.len() {
            return Err("stale UI state handle");
        }
        self.profile.changed_state_writes = self.profile.changed_state_writes.saturating_add(1);
        self.dirty_states.insert(index);
        Ok(())
    }

    fn enter_generic_component(
        &mut self,
        component: String,
        call_site: u64,
        key: Option<Key>,
    ) -> Result<(), &'static str> {
        if !matches!(self.phase, MountPhase::AwaitingRoot { .. }) {
            return Err("UI component scopes may only run while building the mounted root");
        }
        if self.generic_component_stack.len() >= BundleLimits::default().max_static_nesting {
            return Err("UI component scope nesting limit exceeded");
        }
        let parent = self
            .generic_component_stack
            .last()
            .map_or_else(Vec::new, |frame| frame.path.clone());
        let instance = match key {
            Some(key) => GenericComponentIdentity::Key(key),
            None => {
                let occurrence_key = GenericOccurrenceKey {
                    parent: parent.clone(),
                    component: component.clone(),
                    call_site,
                };
                let occurrence = self.generic_occurrences.entry(occurrence_key).or_insert(0);
                let current = *occurrence;
                *occurrence = occurrence
                    .checked_add(1)
                    .ok_or("UI component occurrence space is exhausted")?;
                GenericComponentIdentity::CallSite {
                    id: call_site,
                    occurrence: current,
                }
            }
        };
        let mut path = parent;
        path.push(GenericComponentPathSegment {
            component,
            instance,
        });
        self.generic_component_stack.push(GenericComponentFrame {
            path,
            state_cursor: 0,
        });
        Ok(())
    }

    fn exit_generic_component(&mut self) -> Result<(), &'static str> {
        self.generic_component_stack
            .pop()
            .map(|_| ())
            .ok_or("UI component scope stack is empty")
    }

    fn use_generic_state(&mut self, initial: StateCell) -> Result<u64, &'static str> {
        let frame = self
            .generic_component_stack
            .last_mut()
            .ok_or("UI component scope is missing")?;
        let field = frame.state_cursor;
        frame.state_cursor = frame
            .state_cursor
            .checked_add(1)
            .ok_or("UI component state field space is exhausted")?;
        let state_key = GenericStateKey {
            path: frame.path.clone(),
            field,
        };
        self.generic_state_live.insert(state_key.clone());
        if let Some(index) = self.generic_state_handles.get(&state_key).copied() {
            let slot = self
                .generic_state_cells
                .get(index)
                .ok_or("UI component state index is stale")?;
            let existing = slot
                .cell
                .as_ref()
                .ok_or("UI component state index is stale")?;
            if core::mem::discriminant(existing) != core::mem::discriminant(&initial) {
                return Err("UI component state declaration changed its value type");
            }
            return encode_scoped_state_handle(index, slot.generation);
        }
        if self
            .state_cells
            .len()
            .saturating_add(self.generic_state_handles.len())
            >= MAX_STATE_CELLS
        {
            return Err("UI state declaration limit exceeded");
        }
        let index = if let Some(index) = self.generic_state_free.pop() {
            let slot = &mut self.generic_state_cells[index];
            slot.generation = slot
                .generation
                .checked_add(1)
                .filter(|generation| *generation <= 0x7fff_ffff)
                .ok_or("UI component state generation space is exhausted")?;
            slot.cell = Some(initial);
            index
        } else {
            let index = self.generic_state_cells.len();
            self.generic_state_cells.push(GenericStateSlot {
                generation: 1,
                cell: Some(initial),
            });
            index
        };
        self.generic_state_handles.insert(state_key, index);
        encode_scoped_state_handle(index, self.generic_state_cells[index].generation)
    }
}

fn encode_scoped_state_handle(index: usize, generation: u32) -> Result<u64, &'static str> {
    let encoded_index = u64::try_from(index)
        .ok()
        .and_then(|index| index.checked_add(1))
        .filter(|index| *index <= u64::from(u32::MAX))
        .ok_or("UI component state identity space is exhausted")?;
    if generation == 0 || generation > 0x7fff_ffff {
        return Err("UI component state generation is invalid");
    }
    Ok(SCOPED_STATE_HANDLE_TAG | (u64::from(generation) << 32) | encoded_index)
}

fn decode_scoped_state_handle(handle: u64) -> Result<(usize, u32), &'static str> {
    let index = u32::try_from(handle & u64::from(u32::MAX))
        .ok()
        .and_then(|index| index.checked_sub(1))
        .and_then(|index| usize::try_from(index).ok())
        .ok_or("invalid UI component state handle")?;
    let generation = ((handle & !SCOPED_STATE_HANDLE_TAG) >> 32) as u32;
    if generation == 0 {
        return Err("invalid UI component state handle");
    }
    Ok((index, generation))
}

#[derive(Clone, Debug)]
struct ReloadSnapshot {
    schema: Option<ComponentSchema>,
    cells: Vec<StateCell>,
    generic_cells: BTreeMap<GenericStateKey, StateCell>,
    bundle_cells: BTreeMap<BundleStateKey, StateCell>,
    location: String,
    viewport_width: f64,
    viewport_height: f64,
    scale_factor: f64,
    session_epoch: u64,
}

/// Owns the previous VM-facing UI arena until a replacement module has mounted
/// successfully. Dropping an uncommitted checkpoint restores the previous
/// handlers, mounted tree, state cells, and host-event phase.
pub struct ReloadCheckpoint {
    previous: Option<BuildArena>,
}

impl ReloadCheckpoint {
    pub fn commit(mut self) {
        self.previous.take();
    }
}

impl Drop for ReloadCheckpoint {
    fn drop(&mut self) {
        let Some(previous) = self.previous.take() else {
            return;
        };
        BUILD_ARENA.with(|arena| *arena.borrow_mut() = previous);
    }
}

fn component_schema(component: &ComponentArtifact) -> ComponentSchema {
    ComponentSchema::new(
        component.identity.clone(),
        component
            .states
            .iter()
            .map(|state| StateField::new(state.key.clone(), state.type_fingerprint))
            .collect(),
    )
}

impl BuildArena {
    fn reload_snapshot(&self) -> ReloadSnapshot {
        ReloadSnapshot {
            schema: self.component.as_ref().map(component_schema),
            cells: self.state_cells.clone(),
            generic_cells: self
                .generic_state_handles
                .iter()
                .filter_map(|(key, index)| {
                    self.generic_state_cells
                        .get(*index)
                        .and_then(|slot| slot.cell.as_ref())
                        .cloned()
                        .map(|cell| (key.clone(), cell))
                })
                .collect(),
            bundle_cells: self
                .bundle_state_handles
                .iter()
                .filter_map(|(key, handle)| {
                    self.state(*handle)
                        .ok()
                        .cloned()
                        .map(|cell| (key.clone(), cell))
                })
                .collect(),
            location: self.location.clone(),
            viewport_width: self.viewport_width,
            viewport_height: self.viewport_height,
            scale_factor: self.scale_factor,
            session_epoch: self.session_epoch,
        }
    }

    fn for_reload(
        component: Option<ComponentArtifact>,
        component_bundle: Option<ComponentBundle>,
        snapshot: &ReloadSnapshot,
    ) -> Result<Self, ExternContractError> {
        let session_epoch = snapshot
            .session_epoch
            .checked_add(1)
            .ok_or_else(|| ExternContractError::new("UI session identity space is exhausted"))?;
        let mut arena = Self {
            component,
            component_bundle,
            location: snapshot.location.clone(),
            viewport_width: snapshot.viewport_width,
            viewport_height: snapshot.viewport_height,
            scale_factor: snapshot.scale_factor,
            reload_session_epoch: Some(session_epoch),
            ..Self::default()
        };
        if arena.component_bundle.is_some() {
            for (key, value) in &snapshot.bundle_cells {
                if arena.state_cells.len() >= MAX_STATE_CELLS {
                    return Err(ExternContractError::new(
                        "reloaded UI component state exceeds its cell limit",
                    ));
                }
                let handle = u64::try_from(arena.state_cells.len() + 1).map_err(|_| {
                    ExternContractError::new("UI component state identity space is exhausted")
                })?;
                arena.state_cells.push(value.clone());
                arena.bundle_state_handles.insert(key.clone(), handle);
            }
            return Ok(arena);
        }
        for (key, value) in &snapshot.generic_cells {
            if arena
                .state_cells
                .len()
                .saturating_add(arena.generic_state_cells.len())
                >= MAX_STATE_CELLS
            {
                return Err(ExternContractError::new(
                    "reloaded generic component state exceeds its cell limit",
                ));
            }
            let index = arena.generic_state_cells.len();
            arena.generic_state_cells.push(GenericStateSlot {
                generation: 1,
                cell: Some(value.clone()),
            });
            arena.generic_state_handles.insert(key.clone(), index);
        }
        if arena.component.is_none() && snapshot.schema.is_none() {
            arena
                .reload_cells
                .extend(snapshot.cells.iter().cloned().enumerate());
            return Ok(arena);
        }
        let (Some(component), Some(previous_schema)) =
            (arena.component.as_ref(), snapshot.schema.as_ref())
        else {
            return Ok(arena);
        };
        let next_schema = component_schema(component);
        let plan = plan_reload(previous_schema, &next_schema, ReloadLimits::default())
            .map_err(|error| ExternContractError::new(error.to_string()))?;
        for (next_index, action) in plan.actions.into_iter().enumerate() {
            let StateAction::Preserve { previous_index } = action else {
                continue;
            };
            if let Some(value) = snapshot.cells.get(previous_index as usize) {
                arena.reload_cells.insert(next_index, value.clone());
            }
        }
        Ok(arena)
    }
}

/// Installs a candidate module arena while retaining the complete previous
/// arena for rollback. Compatible state cells are matched by compiler-issued
/// keys and logical type fingerprints; new and changed cells run initializers.
pub fn begin_reload(
    component: Option<ComponentArtifact>,
) -> Result<ReloadCheckpoint, ExternContractError> {
    begin_reload_with_bundle(component, None)
}

pub fn begin_reload_with_bundle(
    component: Option<ComponentArtifact>,
    component_bundle: Option<ComponentBundle>,
) -> Result<ReloadCheckpoint, ExternContractError> {
    BUILD_ARENA.with(|cell| {
        let previous = core::mem::take(&mut *cell.borrow_mut());
        let snapshot = previous.reload_snapshot();
        match BuildArena::for_reload(component, component_bundle, &snapshot) {
            Ok(next) => {
                *cell.borrow_mut() = next;
                Ok(ReloadCheckpoint {
                    previous: Some(previous),
                })
            }
            Err(error) => {
                *cell.borrow_mut() = previous;
                Err(error)
            }
        }
    })
}

thread_local! {
    static BUILD_ARENA: RefCell<BuildArena> = RefCell::new(BuildArena::default());
}

/// Returns the counters for the UI VM/JIT owner executing on this thread.
/// Provider registration and hot replacement start a fresh bounded profile.
pub fn reactive_profile() -> ReactiveProfile {
    BUILD_ARENA.with(|arena| arena.borrow().profile)
}

fn implementation(function: &str) -> Option<(ExternFn, ExternEffects)> {
    let pure = ExternEffects::NONE;
    Some(match function {
        "Mount" => (
            mount,
            ExternEffects::MAY_CALL_CLOSURE_REPLAY.union(ExternEffects::MAY_HOST_REPLAY),
        ),
        "runtimeBegin" => (runtime_begin, ExternEffects::MAY_CALL_CLOSURE_REPLAY),
        "runtimeEnterComponent" => (runtime_enter_component, pure),
        "runtimeExitComponent" => (runtime_exit_component, pure),
        "runtimeCommitAndWait" => (runtime_commit_and_wait, ExternEffects::MAY_HOST_REPLAY),
        "Invalidate" => (invalidate, pure),
        "LocationPath" => (location_path, pure),
        "Navigate" => (navigate, pure),
        "ReplaceLocation" => (replace_location, pure),
        "NavigateBack" => (navigate_back, pure),
        "NavigateForward" => (navigate_forward, pure),
        "runtimeViewportMetrics" => (runtime_viewport_metrics, pure),
        "UseStringState" => (use_string_state, pure),
        "StringStateValue" => (string_state_value, pure),
        "SetStringState" => (set_string_state, pure),
        "UseBoolState" => (use_bool_state, pure),
        "BoolStateValue" => (bool_state_value, pure),
        "SetBoolState" => (set_bool_state, pure),
        "UseIntState" => (use_int_state, pure),
        "IntStateValue" => (int_state_value, pure),
        "SetIntState" => (set_int_state, pure),
        "IntStateAlive" => (int_state_alive, pure),
        "IntStateCommitted" => (int_state_committed, pure),
        "UseFloatState" => (use_float_state, pure),
        "FloatStateValue" => (float_state_value, pure),
        "SetFloatState" => (set_float_state, pure),
        "Fragment" => (fragment, pure),
        "Box" => (box_view, pure),
        "Row" => (row, pure),
        "Column" => (column, pure),
        "Stack" => (stack, pure),
        "Grid" => (grid, pure),
        "Scroll" => (scroll, pure),
        "Text" => (text_view, pure),
        "Image" | "runtimeImage" => (image_view, pure),
        "Canvas" | "runtimeCanvas" => (canvas_view, pure),
        "PlatformView" | "runtimePlatformView" => (platform_view, pure),
        "Button" => (button, pure),
        "runtimeButton" => (runtime_button, pure),
        "TextInput" => (text_input, pure),
        "runtimeTextInput" => (runtime_text_input, pure),
        "TextArea" => (text_area, pure),
        "runtimeTextArea" => (runtime_text_area, pure),
        "Toggle" => (toggle, pure),
        "runtimeToggle" => (runtime_toggle, pure),
        "Slider" => (slider, pure),
        "runtimeSlider" => (runtime_slider, pure),
        "Width" => (width, pure),
        "Height" => (height, pure),
        "MinWidth" => (min_width, pure),
        "MinHeight" => (min_height, pure),
        "MaxWidth" => (max_width, pure),
        "MaxHeight" => (max_height, pure),
        "Flex" => (flex, pure),
        "Gap" => (gap, pure),
        "Padding" => (padding, pure),
        "Background" => (background, pure),
        "Foreground" => (foreground, pure),
        "FontSize" => (font_size, pure),
        "FontWeight" => (font_weight, pure),
        "Align" => (align, pure),
        "Justify" => (justify, pure),
        "GridColumns" => (grid_columns, pure),
        "GridTemplateAreas" => (grid_template_areas, pure),
        "GridArea" => (grid_area, pure),
        "Overflow" => (overflow, pure),
        "Radius" => (radius, pure),
        "BorderColor" => (border_color, pure),
        "BorderWidth" => (border_width, pure),
        "ScrollX" => (scroll_x, pure),
        "ScrollY" => (scroll_y, pure),
        "Disabled" => (disabled, pure),
        "Checked" => (checked, pure),
        "Role" => (role, pure),
        "AccessibleName" => (accessible_name, pure),
        "AccessibleDescription" => (accessible_description, pure),
        "AccessibleValue" => (accessible_value, pure),
        "Required" => (required, pure),
        "Invalid" => (invalid, pure),
        "Selected" => (selected, pure),
        "Expanded" => (expanded, pure),
        "Pressed" => (pressed, pure),
        "Current" => (current, pure),
        "Hidden" => (hidden, pure),
        "AccessibilityHidden" => (accessibility_hidden, pure),
        "Focusable" => (focusable, pure),
        "Source" => (source, pure),
        "ContentType" => (content_type, pure),
        "Fit" => (fit, pure),
        "Opacity" => (opacity, pure),
        "Transform" => (transform, pure),
        "GraphicsProgram" => (graphics_program, pure),
        "MediaState" => (media_state, pure),
        "Poster" => (poster, pure),
        "Modal" => (modal, pure),
        "AutoFocus" => (auto_focus, pure),
        "PointerEvents" => (pointer_events, pure),
        "CapturePointer" => (capture_pointer, pure),
        "FlowDirection" => (flow_direction, pure),
        "Portal" => (portal, pure),
        "FocusRequest" => (focus_request, pure),
        "Key" => (key_view, pure),
        "SelectionStartUTF16" => (selection_start_utf16, pure),
        "SelectionLengthUTF16" => (selection_length_utf16, pure),
        "TestID" => (test_id, pure),
        "OnClick" => (on_click, pure),
        "runtimeOnClick" => (runtime_on_click, pure),
        "OnSubmit" => (on_submit, pure),
        "runtimeOnSubmit" => (runtime_on_submit, pure),
        "OnFocus" => (on_focus, pure),
        "runtimeOnFocus" => (runtime_on_focus, pure),
        "OnBlur" => (on_blur, pure),
        "runtimeOnBlur" => (runtime_on_blur, pure),
        "OnKeyDown" => (on_key_down, pure),
        "runtimeOnKeyDown" => (runtime_on_key_down, pure),
        "OnKeyDownCapture" => (on_key_down_capture, pure),
        "runtimeOnKeyDownCapture" => (runtime_on_key_down_capture, pure),
        "OnKeyUp" => (on_key_up, pure),
        "runtimeOnKeyUp" => (runtime_on_key_up, pure),
        "OnPointerDown" => (on_pointer_down, pure),
        "runtimeOnPointerDown" => (runtime_on_pointer_down, pure),
        "OnPointerMove" => (on_pointer_move, pure),
        "runtimeOnPointerMove" => (runtime_on_pointer_move, pure),
        "OnPointerUp" => (on_pointer_up, pure),
        "runtimeOnPointerUp" => (runtime_on_pointer_up, pure),
        "OnPointerCancel" => (on_pointer_cancel, pure),
        "runtimeOnPointerCancel" => (runtime_on_pointer_cancel, pure),
        "OnScroll" => (on_scroll, pure),
        "runtimeOnScroll" => (runtime_on_scroll, pure),
        "OnCompositionStart" => (on_composition_start, pure),
        "runtimeOnCompositionStart" => (runtime_on_composition_start, pure),
        "OnCompositionUpdate" => (on_composition_update, pure),
        "runtimeOnCompositionUpdate" => (runtime_on_composition_update, pure),
        "OnCompositionEnd" => (on_composition_end, pure),
        "runtimeOnCompositionEnd" => (runtime_on_composition_end, pure),
        "OnSelectionChange" => (on_selection_change, pure),
        "runtimeOnSelectionChange" => (runtime_on_selection_change, pure),
        "OnWheel" => (on_wheel, pure),
        "runtimeOnWheel" => (runtime_on_wheel, pure),
        "OnLayout" => (on_layout, pure),
        "runtimeOnLayout" => (runtime_on_layout, pure),
        _ => return None,
    })
}

fn system_implementation(function: &str) -> Option<(ExternFn, ExternEffects)> {
    let replay = ExternEffects::MAY_HOST_REPLAY;
    Some(match function {
        "runtimeReadClipboard" => (runtime_read_clipboard, replay),
        "runtimeWriteClipboard" => (runtime_write_clipboard, replay),
        "runtimeFileDialog" => (runtime_file_dialog, replay),
        "runtimeMessageDialog" => (runtime_message_dialog, replay),
        "runtimeInstallMenu" => (runtime_install_menu, replay),
        "runtimeBeginFileDrag" => (runtime_begin_file_drag, replay),
        "runtimeWaitEvent" => (runtime_wait_event, replay),
        "runtimeInvokeHost" => (runtime_invoke_host, replay),
        _ => return None,
    })
}

/// Registers only the official UI externs present in one verified module.
/// Calling this before `Vm::load_verified_with_extensions` works for both the
/// interpreter and JIT because the frozen provider table is shared.
pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    // One mounted application owns one UI Island/thread. Registration starts a
    // fresh development session before the provider table is frozen.
    BUILD_ARENA.with(|arena| arena.borrow_mut().reset(None));
    register_extern_definitions(registry, externs)
}

/// Configures one VM/JIT instance from its verified module artifact and
/// registers the official UI extern providers requested by that module.
pub fn register_module(
    registry: &mut ExternRegistry,
    module: &vo_runtime::bytecode::Module,
) -> Result<(), ExternContractError> {
    let component = module_component(module)?;
    let component_bundle = module_component_bundle(module)?;
    BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        arena.reset(component);
        arena.component_bundle = component_bundle;
    });
    register_extern_definitions(registry, &module.externs)
}

fn module_component(
    module: &vo_runtime::bytecode::Module,
) -> Result<Option<ComponentArtifact>, ExternContractError> {
    match module.artifact(COMPONENT_ARTIFACT_NAME) {
        Some(artifact) => {
            if artifact.version != COMPONENT_ARTIFACT_VERSION {
                return Err(ExternContractError::new(format!(
                    "unsupported UI component artifact version {}",
                    artifact.version
                )));
            }
            let component = decode_component_artifact(
                &artifact.payload,
                ArtifactLimits::default(),
                PlanLimits::default(),
            )
            .map_err(|error| ExternContractError::new(error.to_string()))?;
            validate_component_functions(&component, module)?;
            Ok(Some(component))
        }
        None => Ok(None),
    }
}

/// Validates a replacement component and registers its official extern table
/// without touching the currently mounted arena. Call `begin_reload` only
/// after the new bytecode module has passed VM loading.
#[derive(Clone, Debug)]
pub struct PreparedReloadModule {
    pub component: Option<ComponentArtifact>,
    pub component_bundle: Option<ComponentBundle>,
}

pub fn prepare_reload_module(
    registry: &mut ExternRegistry,
    module: &vo_runtime::bytecode::Module,
) -> Result<PreparedReloadModule, ExternContractError> {
    let component = module_component(module)?;
    let component_bundle = module_component_bundle(module)?;
    register_extern_definitions(registry, &module.externs)?;
    Ok(PreparedReloadModule {
        component,
        component_bundle,
    })
}

fn module_component_bundle(
    module: &vo_runtime::bytecode::Module,
) -> Result<Option<ComponentBundle>, ExternContractError> {
    let Some(artifact) = module.artifact(COMPONENT_BUNDLE_ARTIFACT_NAME) else {
        return Ok(None);
    };
    if artifact.version != COMPONENT_BUNDLE_ARTIFACT_VERSION {
        return Err(ExternContractError::new(format!(
            "unsupported UI component bundle artifact version {}",
            artifact.version
        )));
    }
    let bundle = decode_component_bundle(
        &artifact.payload,
        BundleLimits::default(),
        PlanLimits::default(),
    )
    .map_err(|error| ExternContractError::new(error.to_string()))?;
    validate_component_bundle_functions(&bundle, module)?;
    Ok(Some(bundle))
}

fn validate_component_bundle_functions(
    bundle: &ComponentBundle,
    module: &vo_runtime::bytecode::Module,
) -> Result<(), ExternContractError> {
    let functions =
        bundle.definitions.iter().flat_map(|definition| {
            definition
                .states
                .iter()
                .filter_map(|state| state.initializer_func)
                .chain(
                    definition
                        .bindings
                        .iter()
                        .filter_map(|binding| binding.evaluator_func),
                )
                .chain(
                    definition
                        .handlers
                        .iter()
                        .filter_map(|handler| handler.evaluator_func),
                )
                .chain(definition.effects.iter().flat_map(|effect| {
                    core::iter::once(effect.start_func).chain(effect.cleanup_func)
                }))
                .chain(
                    definition
                        .tasks
                        .iter()
                        .flat_map(|task| [task.start_func, task.reducer_func]),
                )
                .chain(definition.lifecycle.mounted_func)
                .chain(definition.lifecycle.updated_func)
                .chain(definition.lifecycle.disposing_func)
        });
    for function in functions {
        let Some(entry) = module.functions.get(function as usize) else {
            return Err(ExternContractError::new(format!(
                "UI component bundle function {function} exceeds module function count {}",
                module.functions.len()
            )));
        };
        if entry.is_closure {
            return Err(ExternContractError::new(format!(
                "UI component bundle entry {} must be statically callable",
                entry.name
            )));
        }
    }
    Ok(())
}

fn validate_component_functions(
    component: &ComponentArtifact,
    module: &vo_runtime::bytecode::Module,
) -> Result<(), ExternContractError> {
    if component.mode != ExecutionMode::Direct {
        return Ok(());
    }
    let functions = component
        .states
        .iter()
        .filter_map(|state| state.initializer_func)
        .chain(
            component
                .slots
                .iter()
                .filter_map(|slot| slot.evaluator_func),
        )
        .chain(
            component
                .handlers
                .iter()
                .filter_map(|handler| handler.evaluator_func),
        );
    for function in functions {
        if function as usize >= module.functions.len() {
            return Err(ExternContractError::new(format!(
                "UI component evaluator function {function} exceeds module function count {}",
                module.functions.len()
            )));
        }
    }
    for state in component
        .states
        .iter()
        .filter(|state| state.has_initializer)
    {
        let function = state
            .initializer_func
            .and_then(|function| module.functions.get(function as usize))
            .expect("direct artifact validation requires a bounded initializer function");
        if function.is_closure || function.param_slots != 0 || function.ret_slots != 1 {
            return Err(ExternContractError::new(format!(
                "UI state initializer {} must be a static () -> one-slot evaluator",
                function.name
            )));
        }
    }
    for binding in &component.slots {
        let function = binding
            .evaluator_func
            .and_then(|function| module.functions.get(function as usize))
            .expect("direct artifact validation requires a bounded slot function");
        let expected_params = u16::try_from(binding.dependencies.len())
            .map_err(|_| ExternContractError::new("UI direct slot dependency count exceeds u16"))?;
        if function.is_closure || function.param_slots != expected_params || function.ret_slots != 1
        {
            return Err(ExternContractError::new(format!(
                "UI slot evaluator {} must be a static {expected_params}-slot -> one-slot function",
                function.name
            )));
        }
    }
    Ok(())
}

fn register_extern_definitions(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    for (id, definition) in unique_extern_providers(externs) {
        let Ok(key) = decode_extern_name(&definition.name) else {
            continue;
        };
        let provider = match key.package() {
            UI_MODULE_PATH => implementation(key.function()),
            UI_SYSTEM_MODULE_PATH => system_implementation(key.function()),
            _ => continue,
        };
        let Some((provider, effects)) = provider else {
            return Err(ExternContractError::new(format!(
                "official UI extern has no VM/JIT provider: {}.{}",
                key.package(),
                key.function()
            )));
        };
        registry.try_register_named_with_effects(
            id as u32,
            definition.name.clone(),
            provider,
            effects,
        )?;
    }
    Ok(())
}

fn mount(call: &mut ExternCallContext<'_>) -> ExternResult {
    // Replay restarts the extern from its call site and retains every earlier
    // closure result. Consume the complete prefix; the newest result belongs
    // to the phase that requested the most recent closure.
    let mut closure_result = None;
    while let Some(result) = call.resume_closure_result() {
        closure_result = Some(result);
    }
    let resume_token = call.take_resume_host_event_token();
    let resume_data = resume_token.and_then(|_| call.take_resume_host_event_data());
    let phase = BUILD_ARENA.with(|arena| arena.borrow().phase);

    match phase {
        MountPhase::Idle => start_root_render(call, true),
        MountPhase::AwaitingRoot { initial } => {
            let Some(result) = closure_result else {
                return ExternResult::Panic(
                    "UI root replay did not contain a closure result".to_string(),
                );
            };
            finish_root_render(call, initial, &result)
        }
        MountPhase::WaitingEvent { token } => {
            if resume_token != Some(token) {
                return ExternResult::Panic("UI host event resumed with a stale token".to_string());
            }
            let Some(bytes) = resume_data else {
                return ExternResult::Panic("UI host event contained no payload".to_string());
            };
            dispatch_event(call, &bytes)
        }
        MountPhase::AwaitingHandler => {
            if closure_result.is_none() {
                return ExternResult::Panic(
                    "UI handler replay did not contain a closure result".to_string(),
                );
            }
            start_root_render(call, false)
        }
        MountPhase::EvaluatingSlots
        | MountPhase::EvaluatingBundle
        | MountPhase::BundleHandlerReady
        | MountPhase::AwaitingBundleHandler
        | MountPhase::AwaitingDirectCommit => {
            ExternResult::Panic("legacy UI Mount entered a direct-update phase".to_string())
        }
    }
}

fn runtime_begin(call: &mut ExternCallContext<'_>) -> ExternResult {
    let initial = call.arg_bool(0);
    let mut closure_result = None;
    while let Some(result) = call.resume_closure_result() {
        closure_result = Some(result);
    }
    let mut phase = BUILD_ARENA.with(|arena| arena.borrow().phase);
    if phase == MountPhase::BundleHandlerReady {
        if initial || closure_result.is_some() {
            return ExternResult::Panic(
                "UI component handler began with an invalid replay state".to_string(),
            );
        }
        let pending = BUILD_ARENA.with(|arena| {
            let mut arena = arena.borrow_mut();
            let (handler, event) = arena
                .pending_component_event
                .take()
                .ok_or_else(|| "UI component event is missing".to_string())?;
            let lease = arena
                .bundle_handlers
                .get(&handler)
                .copied()
                .ok_or_else(|| "UI component handler closure is missing".to_string())?;
            arena.bundle_state_checkpoint = Some(BundleStateCheckpoint::capture(&arena));
            arena.phase = MountPhase::AwaitingBundleHandler;
            Ok((lease, event))
        });
        let (lease, event) = match pending {
            Ok(pending) => pending,
            Err(message) => return fail_bundle_evaluation(call, message),
        };
        let closure_ref = match call.gc_lease_root(lease) {
            Ok(reference) => reference,
            Err(error) => {
                return fail_bundle_evaluation(
                    call,
                    format!("failed to resolve UI component handler: {error}"),
                );
            }
        };
        return ExternResult::CallClosure {
            closure_ref,
            args: event_args(call, &event),
        };
    }
    if phase == MountPhase::AwaitingBundleHandler {
        if initial || closure_result.is_none() {
            return fail_bundle_evaluation(
                call,
                "UI component handler replay contained no result".to_string(),
            );
        }
        let bundle = BUILD_ARENA.with(|arena| {
            let mut arena = arena.borrow_mut();
            arena.phase = MountPhase::Idle;
            if arena
                .component
                .as_ref()
                .is_some_and(|component| component.mode == ExecutionMode::Direct)
            {
                return Ok(None);
            }
            arena
                .component_bundle
                .as_ref()
                .filter(|bundle| bundle_direct_supported(bundle))
                .cloned()
                .map(Some)
                .ok_or_else(|| "UI component bundle is unavailable after its handler".to_string())
        });
        match bundle {
            Ok(Some(bundle)) => return start_bundle_evaluation(call, false, bundle),
            Ok(None) => phase = MountPhase::Idle,
            Err(message) => return fail_bundle_evaluation(call, message),
        }
    }
    if phase == MountPhase::EvaluatingBundle {
        let Some(result) = closure_result else {
            return ExternResult::Panic(
                "UI component bundle evaluator replay contained no result".to_string(),
            );
        };
        return advance_bundle_evaluation(call, Some(result));
    }
    if phase == MountPhase::EvaluatingSlots {
        let Some(result) = closure_result else {
            return ExternResult::Panic(
                "UI direct evaluator replay contained no result".to_string(),
            );
        };
        return advance_direct_update(call, Some(result));
    }

    let direct = BUILD_ARENA.with(|arena| {
        arena
            .borrow()
            .component
            .as_ref()
            .is_some_and(|component| component.mode == ExecutionMode::Direct)
    });
    if !initial && direct {
        if phase != MountPhase::Idle {
            return ExternResult::Panic("UI direct update began in an invalid phase".to_string());
        }
        let prepared = BUILD_ARENA.with(|arena| {
            let mut arena = arena.borrow_mut();
            if arena.session_epoch == 0 || arena.runtime.is_none() {
                return Err("UI direct update has no mounted root".to_string());
            }
            let component = arena
                .component
                .as_ref()
                .ok_or_else(|| "UI direct update lost its component artifact".to_string())?;
            let bindings = component
                .slots
                .iter()
                .enumerate()
                .filter_map(|(index, binding)| {
                    (arena.force_direct_slots
                        || binding.dependencies.is_empty()
                        || binding
                            .dependencies
                            .iter()
                            .any(|state| arena.dirty_states.contains(state)))
                    .then_some(index)
                })
                .collect::<Vec<_>>();
            arena.profile.direct_update_turns = arena.profile.direct_update_turns.saturating_add(1);
            arena.profile.scheduled_bindings = arena
                .profile
                .scheduled_bindings
                .saturating_add(bindings.len() as u64);
            arena.direct_update = Some(DirectUpdate {
                bindings,
                cursor: 0,
                awaiting: None,
                updates: Vec::new(),
            });
            arena.direct_batch = None;
            arena.phase = MountPhase::EvaluatingSlots;
            Ok(())
        });
        if let Err(message) = prepared {
            return ExternResult::Panic(message);
        }
        return advance_direct_update(call, None);
    }

    let component_bundle = BUILD_ARENA.with(|arena| {
        arena
            .borrow()
            .component_bundle
            .as_ref()
            .filter(|bundle| bundle_direct_supported(bundle))
            .cloned()
    });
    if let Some(bundle) = component_bundle {
        if phase != MountPhase::Idle {
            return ExternResult::Panic(
                "UI component bundle evaluation began in an invalid phase".to_string(),
            );
        }
        return start_bundle_evaluation(call, initial, bundle);
    }

    let leases = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        match (initial, arena.phase) {
            (true, MountPhase::Idle) | (false, MountPhase::Idle) => {}
            _ => return Err("UI render transaction began in an invalid phase".to_string()),
        }
        if initial {
            if arena.session_epoch != 0 || arena.runtime.is_some() {
                return Err("UI initial render transaction was started twice".to_string());
            }
            let epoch = match arena.reload_session_epoch.take() {
                Some(epoch) => epoch,
                None => call
                    .try_next_host_event_token()
                    .ok_or_else(|| "UI session identity space is exhausted".to_string())?,
            };
            arena.session_epoch = epoch;
        } else if arena.session_epoch == 0 || arena.runtime.is_none() {
            return Err("UI update render transaction has no mounted root".to_string());
        }
        let leases = core::mem::take(&mut arena.handlers);
        arena.begin_render(initial);
        Ok(leases)
    });
    let leases = match leases {
        Ok(leases) => leases,
        Err(message) => return ExternResult::Panic(message),
    };
    for lease in leases {
        if let Err(error) = call.gc_release_lease(lease) {
            return ExternResult::Panic(format!("failed to release stale UI handler: {error}"));
        }
    }
    call.ret_bool(0, false);
    ExternResult::Ok
}

fn start_bundle_evaluation(
    call: &mut ExternCallContext<'_>,
    initial: bool,
    bundle: ComponentBundle,
) -> ExternResult {
    let stale_handlers = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        arena.bundle_pending_handler = None;
        core::mem::take(&mut arena.bundle_next_handlers)
            .into_values()
            .collect::<Vec<_>>()
    });
    for lease in stale_handlers {
        if let Err(error) = call.gc_release_lease(lease) {
            return fail_bundle_evaluation(
                call,
                format!("failed to release abandoned UI component handler: {error}"),
            );
        }
    }
    let prepared = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if initial {
            if arena.session_epoch != 0 || arena.runtime.is_some() {
                return Err("UI component bundle mount was started twice".to_string());
            }
            arena.profile.root_evaluations = arena.profile.root_evaluations.saturating_add(1);
            arena.session_epoch = match arena.reload_session_epoch.take() {
                Some(epoch) => epoch,
                None => call
                    .try_next_host_event_token()
                    .ok_or_else(|| "UI session identity space is exhausted".to_string())?,
            };
        } else if arena.session_epoch == 0 || arena.runtime.is_none() {
            return Err("UI component bundle update has no mounted root".to_string());
        }
        arena.bundle_evaluation = Some(BundleEvaluation::new(bundle, initial)?);
        arena.phase = MountPhase::EvaluatingBundle;
        Ok(())
    });
    match prepared {
        Ok(()) => advance_bundle_evaluation(call, None),
        Err(message) => fail_bundle_evaluation(call, message),
    }
}

fn advance_bundle_evaluation(
    call: &mut ExternCallContext<'_>,
    mut completed: Option<Vec<u64>>,
) -> ExternResult {
    if let Some(key) = BUILD_ARENA.with(|arena| arena.borrow_mut().bundle_pending_handler.take()) {
        let Some(result) = completed.as_ref() else {
            return fail_bundle_evaluation(
                call,
                "UI component handler evaluator replay contained no result".to_string(),
            );
        };
        let [raw] = result.as_slice() else {
            return fail_bundle_evaluation(
                call,
                format!(
                    "UI component handler evaluator returned {} slots; expected one closure",
                    result.len()
                ),
            );
        };
        let reference = *raw as usize as GcRef;
        if reference.is_null() {
            return fail_bundle_evaluation(
                call,
                "UI component handler evaluator returned a nil closure".to_string(),
            );
        }
        let lease = match call.gc_lease(reference) {
            Ok(lease) => lease,
            Err(error) => {
                return fail_bundle_evaluation(
                    call,
                    format!("failed to retain UI component handler: {error}"),
                );
            }
        };
        let previous =
            BUILD_ARENA.with(|arena| arena.borrow_mut().bundle_next_handlers.insert(key, lease));
        if let Some(previous) = previous {
            if let Err(error) = call.gc_release_lease(previous) {
                return fail_bundle_evaluation(
                    call,
                    format!("failed to release replaced UI component handler: {error}"),
                );
            }
        }
    }
    loop {
        let request = BUILD_ARENA.with(|arena| {
            let mut arena = arena.borrow_mut();
            if let Some((handle, kind)) = arena.bundle_pending_state.take() {
                let result = completed.as_ref().ok_or_else(|| {
                    "UI component state initializer replay contained no result".to_string()
                })?;
                let value = bundle_state_cell(kind, result)?;
                let component_value = bundle_component_value(&value);
                let state = arena
                    .state_mut(handle)
                    .map_err(|message| message.to_string())?;
                if core::mem::discriminant(state) != core::mem::discriminant(&value) {
                    return Err("UI component state changed its declared value kind".to_string());
                }
                *state = value;
                arena
                    .bundle_evaluation
                    .as_mut()
                    .ok_or_else(|| "UI component bundle evaluation state is missing".to_string())?
                    .publish_new_state_value(component_value)?;
            }
            arena
                .bundle_evaluation
                .as_mut()
                .ok_or_else(|| "UI component bundle evaluation state is missing".to_string())?
                .advance(completed.take())
        });
        let request = match request {
            Ok(request) => request,
            Err(message) => return fail_bundle_evaluation(call, message),
        };
        let Some(request) = request else {
            break;
        };
        if let Some((key, kind)) = &request.state {
            let state = BUILD_ARENA.with(|arena| {
                let mut arena = arena.borrow_mut();
                if let Some(handle) = arena.bundle_state_handles.get(key).copied() {
                    let expected = bundle_state_placeholder(*kind)?;
                    let existing = arena.state(handle).map_err(|message| message.to_string())?;
                    if core::mem::discriminant(existing) != core::mem::discriminant(&expected) {
                        return Err(
                            "UI component state key changed its declared value kind".to_string()
                        );
                    }
                    let component_value = bundle_component_value(existing);
                    arena
                        .bundle_evaluation
                        .as_mut()
                        .ok_or_else(|| {
                            "UI component bundle evaluation state is missing".to_string()
                        })?
                        .bind_reused_state(handle, component_value)?;
                    return Ok(false);
                }
                if arena.state_cells.len() >= MAX_STATE_CELLS {
                    return Err("UI component state declaration limit exceeded".to_string());
                }
                let handle = u64::try_from(arena.state_cells.len() + 1)
                    .map_err(|_| "UI component state identity space is exhausted".to_string())?;
                arena.state_cells.push(bundle_state_placeholder(*kind)?);
                arena.bundle_state_handles.insert(key.clone(), handle);
                arena
                    .bundle_evaluation
                    .as_mut()
                    .ok_or_else(|| "UI component bundle evaluation state is missing".to_string())?
                    .bind_new_state(handle)?;
                arena.bundle_pending_state = Some((handle, *kind));
                Ok(true)
            });
            match state {
                Ok(false) => continue,
                Ok(true) => {}
                Err(message) => return fail_bundle_evaluation(call, message),
            }
        }
        if let Some(key) = request.handler.clone() {
            BUILD_ARENA.with(|arena| arena.borrow_mut().bundle_pending_handler = Some(key));
        }
        BUILD_ARENA.with(|arena| {
            let mut arena = arena.borrow_mut();
            if arena
                .bundle_evaluation
                .as_ref()
                .is_some_and(|evaluation| !evaluation.initial)
            {
                arena.profile.evaluator_calls = arena.profile.evaluator_calls.saturating_add(1);
            }
        });
        let closure_ref = match closure::try_create(call.gc(), request.function, 0) {
            Ok(reference) => reference,
            Err(error) => {
                return fail_bundle_evaluation(
                    call,
                    format!("failed to allocate UI component evaluator closure: {error:?}"),
                );
            }
        };
        call.gc().mark_allocated_for_scan(closure_ref);
        return ExternResult::CallClosure {
            closure_ref,
            args: request.arguments,
        };
    }

    let completed = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let mut evaluation = arena
            .bundle_evaluation
            .take()
            .ok_or_else(|| "UI component bundle evaluation state is missing".to_string())?;
        let initial = evaluation.initial;
        let bundle = evaluation.bundle.clone();
        let root_draft = evaluation.take_root()?;
        let batch = if initial {
            let epoch = arena.session_epoch;
            let root = NodeId::new(0, 1);
            let renderer = HeadlessRenderer::new(epoch, root, ProtocolLimits::default());
            let mut runtime = ComponentTemplateRuntime::new(
                renderer,
                epoch,
                root,
                bundle,
                BundleLimits::default(),
                vo_ui_runtime::ComponentForestLimits::default(),
            )
            .map_err(|error| format!("UI component runtime admission failed: {error}"))?;
            let commit = runtime
                .mount(
                    root_draft.props,
                    root_draft.state,
                    root_draft.slots,
                    root_draft.children,
                )
                .map_err(|error| format!("UI component mount failed: {error}"))?;
            let batch = commit.batch;
            arena.runtime = Some(MountedRuntime::Component(Box::new(runtime)));
            batch
        } else {
            let runtime = match arena.runtime.as_mut() {
                Some(MountedRuntime::Component(runtime)) => runtime,
                _ => return Err("UI component update lost its mounted forest".to_string()),
            };
            runtime
                .update(
                    root_draft.props,
                    root_draft.state,
                    root_draft.slots,
                    root_draft.children,
                )
                .map_err(|error| format!("UI component update failed: {error}"))?
                .batch
        };
        let runtime = match arena.runtime.as_ref() {
            Some(MountedRuntime::Component(runtime)) => runtime,
            _ => return Err("UI component commit lost its mounted forest".to_string()),
        };
        let mut handlers = BTreeMap::new();
        for (key, lease) in &arena.bundle_next_handlers {
            let path = key
                .path
                .iter()
                .copied()
                .map(ComponentCallSiteId::new)
                .collect::<Vec<_>>();
            let handler = runtime
                .handler_at_path(&path, key.site)
                .ok_or_else(|| "UI component handler path is not mounted".to_string())?;
            if handlers.insert(handler, *lease).is_some() {
                return Err("UI component handler identity is duplicated".to_string());
            }
        }
        arena.bundle_next_handlers.clear();
        let previous_handlers = core::mem::replace(&mut arena.bundle_handlers, handlers);
        arena.bundle_state_checkpoint = None;
        arena.profile.record_batch(Some(&batch));
        let encoded = encode_batch(&batch, ProtocolLimits::default())
            .map_err(|error| format!("UI component protocol encoding failed: {error}"))?;
        arena.direct_batch = Some(encoded);
        arena.direct_commit_initial = initial;
        arena.phase = MountPhase::AwaitingDirectCommit;
        Ok(previous_handlers.into_values().collect::<Vec<_>>())
    });
    match completed {
        Ok(previous_handlers) => {
            for lease in previous_handlers {
                if let Err(error) = call.gc_release_lease(lease) {
                    return ExternResult::Panic(format!(
                        "failed to release stale UI component handler: {error}"
                    ));
                }
            }
            call.ret_bool(0, true);
            ExternResult::Ok
        }
        Err(message) => fail_bundle_evaluation(call, message),
    }
}

fn fail_bundle_evaluation(call: &mut ExternCallContext<'_>, mut message: String) -> ExternResult {
    let leases = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if let Some(previous) = arena.bundle_state_checkpoint.take() {
            previous.restore(&mut arena);
        }
        arena.bundle_evaluation = None;
        arena.bundle_pending_state = None;
        arena.bundle_pending_handler = None;
        arena.pending_component_event = None;
        arena.phase = MountPhase::Idle;
        core::mem::take(&mut arena.bundle_next_handlers)
            .into_values()
            .collect::<Vec<_>>()
    });
    for lease in leases {
        if let Err(error) = call.gc_release_lease(lease) {
            message.push_str(&format!("; failed to release candidate handler: {error}"));
        }
    }
    ExternResult::Panic(message)
}

fn bundle_state_placeholder(kind: StateValueKind) -> Result<StateCell, String> {
    match kind {
        StateValueKind::String => Ok(StateCell::String(String::new())),
        StateValueKind::Bool => Ok(StateCell::Bool(false)),
        StateValueKind::Int => Ok(StateCell::Int(0)),
        StateValueKind::Float => Ok(StateCell::Float(0.0)),
        StateValueKind::Opaque => {
            Err("opaque component state requires the generic runtime".to_string())
        }
    }
}

fn bundle_state_cell(kind: StateValueKind, result: &[u64]) -> Result<StateCell, String> {
    let [raw] = result else {
        return Err(format!(
            "UI component state initializer returned {} slots; expected one",
            result.len()
        ));
    };
    match kind {
        StateValueKind::String => Ok(StateCell::String(direct_string(*raw)?)),
        StateValueKind::Bool => Ok(StateCell::Bool(*raw != 0)),
        StateValueKind::Int => Ok(StateCell::Int(*raw as i64)),
        StateValueKind::Float => {
            let value = f64::from_bits(*raw);
            if value.is_finite() {
                Ok(StateCell::Float(value))
            } else {
                Err("UI component float state requires a finite initial value".to_string())
            }
        }
        StateValueKind::Opaque => {
            Err("opaque component state requires the generic runtime".to_string())
        }
    }
}

fn bundle_component_value(state: &StateCell) -> ComponentValue {
    match state {
        StateCell::String(value) => ComponentValue::Text(value.clone()),
        StateCell::Bool(value) => ComponentValue::Bool(*value),
        StateCell::Int(value) => ComponentValue::Int(*value),
        StateCell::Float(value) => ComponentValue::from_float(*value),
    }
}

fn advance_direct_update(
    call: &mut ExternCallContext<'_>,
    completed: Option<Vec<u64>>,
) -> ExternResult {
    if let Some(result) = completed {
        let binding = BUILD_ARENA.with(|arena| {
            let mut arena = arena.borrow_mut();
            let update = arena
                .direct_update
                .as_mut()
                .ok_or_else(|| "UI direct evaluator state is missing".to_string())?;
            let binding = update
                .awaiting
                .take()
                .ok_or_else(|| "UI direct evaluator replay has no pending binding".to_string())?;
            let component = arena
                .component
                .as_ref()
                .ok_or_else(|| "UI direct evaluator lost its component artifact".to_string())?
                .clone();
            Ok((binding, component))
        });
        let (binding, component) = match binding {
            Ok(binding) => binding,
            Err(message) => return ExternResult::Panic(message),
        };
        let values = match direct_slot_values(&component, binding, &result) {
            Ok(values) => values,
            Err(message) => return ExternResult::Panic(message),
        };
        BUILD_ARENA.with(|arena| {
            if let Some(update) = arena.borrow_mut().direct_update.as_mut() {
                update.updates.extend(values);
            }
        });
    }

    let request = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let component = arena
            .component
            .as_ref()
            .ok_or_else(|| "UI direct update lost its component artifact".to_string())?
            .clone();
        let update = arena
            .direct_update
            .as_mut()
            .ok_or_else(|| "UI direct evaluator state is missing".to_string())?;
        let Some(&binding_index) = update.bindings.get(update.cursor) else {
            return Ok(None);
        };
        update.cursor += 1;
        update.awaiting = Some(binding_index);
        arena.profile.evaluator_calls = arena.profile.evaluator_calls.saturating_add(1);
        let binding = &component.slots[binding_index];
        let function = binding
            .evaluator_func
            .ok_or_else(|| "UI direct slot is missing its evaluator function".to_string())?;
        let mut arguments = Vec::with_capacity(binding.dependencies.len());
        for dependency in &binding.dependencies {
            if *dependency as usize >= arena.state_cells.len() {
                return Err(format!(
                    "UI direct slot references unavailable state cell {dependency}"
                ));
            }
            arguments.push(u64::from(*dependency) + 1);
        }
        if arena.evaluator_leases.len() <= binding_index {
            arena.evaluator_leases.resize(binding_index + 1, None);
        }
        Ok(Some((
            binding_index,
            function,
            arguments,
            arena.evaluator_leases[binding_index],
        )))
    });
    let request = match request {
        Ok(request) => request,
        Err(message) => return ExternResult::Panic(message),
    };
    if let Some((binding, function, arguments, lease)) = request {
        let closure_ref = if let Some(lease) = lease {
            match call.gc_lease_root(lease) {
                Ok(reference) => reference,
                Err(error) => {
                    return ExternResult::Panic(format!(
                        "failed to resolve UI direct evaluator closure: {error}"
                    ));
                }
            }
        } else {
            let reference = match closure::try_create(call.gc(), function, 0) {
                Ok(reference) => reference,
                Err(error) => {
                    return ExternResult::Panic(format!(
                        "failed to allocate UI direct evaluator closure: {error:?}"
                    ));
                }
            };
            call.gc().mark_allocated_for_scan(reference);
            let lease = match call.gc_lease(reference) {
                Ok(lease) => lease,
                Err(error) => {
                    return ExternResult::Panic(format!(
                        "failed to retain UI direct evaluator closure: {error}"
                    ));
                }
            };
            BUILD_ARENA.with(|arena| {
                arena.borrow_mut().evaluator_leases[binding] = Some(lease);
            });
            reference
        };
        return ExternResult::CallClosure {
            closure_ref,
            args: arguments,
        };
    }

    let completed = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let update = arena
            .direct_update
            .take()
            .ok_or_else(|| "UI direct evaluator state is missing".to_string())?;
        arena.profile.submitted_slots = arena
            .profile
            .submitted_slots
            .saturating_add(update.updates.len() as u64);
        let (mutation_count, encoded) = match arena.runtime.as_mut() {
            Some(MountedRuntime::Template(runtime)) => {
                let batch = runtime
                    .update_slots_in_place(update.updates)
                    .map_err(|error| format!("UI direct slot update failed: {error}"))?;
                let mutation_count = batch.map(|batch| batch.mutations.len());
                let encoded = batch
                    .map(|batch| encode_batch(batch, ProtocolLimits::default()))
                    .transpose()
                    .map_err(|error| format!("UI direct protocol encoding failed: {error}"))?;
                (mutation_count, encoded)
            }
            Some(MountedRuntime::Component(runtime)) => {
                let batch = runtime
                    .update_root_slots(update.updates)
                    .map_err(|error| format!("UI component slot update failed: {error}"))?;
                let mutation_count = batch.as_ref().map(|batch| batch.mutations.len());
                let encoded = batch
                    .as_ref()
                    .map(|batch| encode_batch(batch, ProtocolLimits::default()))
                    .transpose()
                    .map_err(|error| format!("UI direct protocol encoding failed: {error}"))?;
                (mutation_count, encoded)
            }
            Some(MountedRuntime::Generic(_)) => {
                return Err("UI direct component has a generic mounted runtime".to_string());
            }
            None => return Err("UI direct update has no mounted runtime".to_string()),
        };
        if let Some(mutation_count) = mutation_count {
            arena.profile.emitted_revisions = arena.profile.emitted_revisions.saturating_add(1);
            arena.profile.emitted_mutations = arena
                .profile
                .emitted_mutations
                .saturating_add(mutation_count as u64);
        } else {
            arena.profile.no_op_updates = arena.profile.no_op_updates.saturating_add(1);
        }
        arena.direct_batch = encoded;
        arena.direct_commit_initial = false;
        arena.dirty_states.clear();
        arena.force_direct_slots = false;
        arena.phase = MountPhase::AwaitingDirectCommit;
        Ok(())
    });
    match completed {
        Ok(()) => {
            call.ret_bool(0, true);
            ExternResult::Ok
        }
        Err(message) => ExternResult::Panic(message),
    }
}

fn direct_slot_values(
    component: &ComponentArtifact,
    binding_index: usize,
    result: &[u64],
) -> Result<Vec<(SlotId, SlotValue)>, String> {
    let [raw] = result else {
        return Err(format!(
            "UI direct evaluator {binding_index} returned {} slots; expected one",
            result.len()
        ));
    };
    let binding = component
        .slots
        .get(binding_index)
        .ok_or_else(|| format!("UI direct slot binding {binding_index} is missing"))?;
    let mut values = Vec::with_capacity(binding.slots.len());
    for slot in &binding.slots {
        let value = match component.plan.slot_kind(*slot) {
            Some(SlotKind::Text) => SlotValue::Text(direct_string(*raw)?),
            Some(SlotKind::Property) => {
                let (target, property) = component
                    .plan
                    .update_sites(*slot)
                    .into_iter()
                    .flatten()
                    .find_map(|site| match site.mutation {
                        DirectMutation::SetProperty { target, property } => {
                            Some((target, property))
                        }
                        DirectMutation::SetText { .. } => None,
                    })
                    .ok_or_else(|| {
                        format!(
                            "UI direct property slot {} has no update site",
                            slot.index()
                        )
                    })?;
                let primitive = match component.plan.node(target).kind {
                    vo_ui_plan::TemplateNodeKind::Element(primitive) => Some(primitive),
                    vo_ui_plan::TemplateNodeKind::Text => None,
                };
                SlotValue::Property(direct_property_value(property, primitive, *raw)?)
            }
            None => return Err(format!("UI direct slot {} is invalid", slot.index())),
        };
        values.push((*slot, value));
    }
    Ok(values)
}

fn direct_string(raw: u64) -> Result<String, String> {
    let reference = raw as usize as GcRef;
    if reference.is_null() {
        return Ok(String::new());
    }
    // The verified evaluator return layout marks this slot as a string
    // reference, and the replay payload roots it for this complete call.
    unsafe { string::try_to_rust_string(reference) }
        .map_err(|error| format!("UI direct evaluator returned invalid UTF-8: {error}"))
}

fn direct_property_value(
    property: PropertyId,
    primitive: Option<Primitive>,
    raw: u64,
) -> Result<Value, String> {
    let value = match property {
        PropertyId::WIDTH
        | PropertyId::HEIGHT
        | PropertyId::MIN_WIDTH
        | PropertyId::MIN_HEIGHT
        | PropertyId::MAX_WIDTH
        | PropertyId::MAX_HEIGHT
        | PropertyId::GAP
        | PropertyId::PADDING
        | PropertyId::FONT_SIZE
        | PropertyId::RADIUS
        | PropertyId::BORDER_WIDTH => Value::Length(Length::Px(f64::from_bits(raw) as f32)),
        PropertyId::FLEX
        | PropertyId::SCROLL_X
        | PropertyId::SCROLL_Y
        | PropertyId::MIN_VALUE
        | PropertyId::MAX_VALUE
        | PropertyId::STEP_VALUE => Value::F64(f64::from_bits(raw)),
        PropertyId::BACKGROUND | PropertyId::FOREGROUND | PropertyId::BORDER_COLOR => {
            Value::Color(raw as u32)
        }
        PropertyId::FONT_WEIGHT
        | PropertyId::SELECTION_START_UTF16
        | PropertyId::SELECTION_LENGTH_UTF16 => Value::I64(raw as i64),
        PropertyId::DISABLED
        | PropertyId::CHECKED
        | PropertyId::REQUIRED
        | PropertyId::INVALID
        | PropertyId::SELECTED
        | PropertyId::EXPANDED
        | PropertyId::PRESSED
        | PropertyId::HIDDEN
        | PropertyId::ACCESSIBILITY_HIDDEN
        | PropertyId::FOCUSABLE
        | PropertyId::MODAL
        | PropertyId::AUTO_FOCUS
        | PropertyId::POINTER_CAPTURE => Value::Bool(raw != 0),
        PropertyId::FLOW_DIRECTION | PropertyId::PORTAL_LAYER | PropertyId::FOCUS_REQUEST => {
            Value::I64(raw as i64)
        }
        PropertyId::VALUE if primitive == Some(Primitive::Slider) => {
            Value::F64(f64::from_bits(raw))
        }
        PropertyId::ALIGN
        | PropertyId::JUSTIFY
        | PropertyId::PLACEHOLDER
        | PropertyId::ROLE
        | PropertyId::ACCESSIBLE_NAME
        | PropertyId::TEST_ID
        | PropertyId::GRID_COLUMNS
        | PropertyId::OVERFLOW
        | PropertyId::ACCESSIBLE_DESCRIPTION
        | PropertyId::GRID_TEMPLATE_AREAS
        | PropertyId::GRID_AREA
        | PropertyId::POINTER_EVENTS => Value::Text(direct_string(raw)?),
        PropertyId::CURRENT | PropertyId::VALUE => Value::Text(direct_string(raw)?),
        _ => {
            return Err(format!(
                "UI direct evaluator cannot decode property {}",
                property.0
            ));
        }
    };
    Ok(value)
}

fn runtime_commit_and_wait(call: &mut ExternCallContext<'_>) -> ExternResult {
    let resume_token = call.take_resume_host_event_token();
    let resume_data = resume_token.and_then(|_| call.take_resume_host_event_data());
    let phase = BUILD_ARENA.with(|arena| arena.borrow().phase);
    match phase {
        MountPhase::AwaitingDirectCommit => {
            let expected_initial = BUILD_ARENA.with(|arena| arena.borrow().direct_commit_initial);
            if call.arg_bool(1) != expected_initial || call.arg_u64(0) != 0 {
                return ExternResult::Panic(
                    "UI direct commit received an invalid root transaction".to_string(),
                );
            }
            let batch = BUILD_ARENA.with(|arena| arena.borrow_mut().direct_batch.take());
            if let Some(batch) = batch {
                call.set_host_output(batch);
            }
            wait_for_event(call)
        }
        MountPhase::AwaitingRoot { initial } => {
            if resume_token.is_some() {
                return ExternResult::Panic(
                    "UI render commit received an unexpected replay token".to_string(),
                );
            }
            if call.arg_bool(1) != initial {
                return ExternResult::Panic(
                    "UI render commit disagrees with its transaction phase".to_string(),
                );
            }
            finish_root_render(call, initial, &[call.arg_u64(0)])
        }
        MountPhase::WaitingEvent { token } => {
            if resume_token != Some(token) {
                return ExternResult::Panic("UI host event resumed with a stale token".to_string());
            }
            let Some(bytes) = resume_data else {
                return ExternResult::Panic("UI host event contained no payload".to_string());
            };
            return_event(call, &bytes)
        }
        MountPhase::Idle
        | MountPhase::AwaitingHandler
        | MountPhase::EvaluatingSlots
        | MountPhase::EvaluatingBundle
        | MountPhase::BundleHandlerReady
        | MountPhase::AwaitingBundleHandler => {
            ExternResult::Panic("UI render commit ran outside an active transaction".to_string())
        }
    }
}

fn invalidate(_call: &mut ExternCallContext<'_>) -> ExternResult {
    BUILD_ARENA.with(|arena| arena.borrow_mut().invalidation_pending = true);
    ExternResult::Ok
}

fn runtime_viewport_metrics(call: &mut ExternCallContext<'_>) -> ExternResult {
    let (width, height, scale) = BUILD_ARENA.with(|arena| {
        let arena = arena.borrow();
        (
            arena.viewport_width,
            arena.viewport_height,
            arena.scale_factor,
        )
    });
    call.ret_f64(0, width);
    call.ret_f64(1, height);
    call.ret_f64(2, scale);
    ExternResult::Ok
}

/// Updates renderer-owned viewport inputs and coalesces one adaptive render.
pub fn set_platform_viewport(
    width: f64,
    height: f64,
    scale_factor: f64,
    invalidate: bool,
) -> Result<bool, &'static str> {
    if !width.is_finite()
        || !height.is_finite()
        || !scale_factor.is_finite()
        || width < 0.0
        || height < 0.0
        || scale_factor <= 0.0
    {
        return Err("UI platform viewport metrics are invalid");
    }
    BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let changed = arena.viewport_width != width
            || arena.viewport_height != height
            || arena.scale_factor != scale_factor;
        if changed {
            arena.viewport_width = width;
            arena.viewport_height = height;
            arena.scale_factor = scale_factor;
            arena.invalidation_pending |= invalidate;
        }
        Ok(changed)
    })
}

/// Sets the initial renderer-owned application location before a VM turn.
/// SSG and SSR adapters use this to render each declared route through the
/// same navigation contract as an activated browser or desktop application.
pub fn set_platform_location(path: &str) -> Result<bool, &'static str> {
    if !valid_location(path) {
        return Err("UI platform location is invalid");
    }
    BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let changed = arena.location != path;
        if changed {
            arena.location.clear();
            arena.location.push_str(path);
        }
        Ok(changed)
    })
}

/// Takes one coalesced worker-goroutine wake request for the active UI Island.
/// Platform loops call this only after a VM scheduler turn has completed.
pub fn take_invalidation_request() -> bool {
    BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        core::mem::take(&mut arena.invalidation_pending)
    })
}

fn valid_location(path: &str) -> bool {
    !path.is_empty()
        && path.len() <= MAX_LOCATION_BYTES
        && path.starts_with('/')
        && !path.starts_with("//")
        && !path
            .bytes()
            .any(|byte| byte == b'\\' || byte.is_ascii_control())
}

fn location_path(call: &mut ExternCallContext<'_>) -> ExternResult {
    let location = BUILD_ARENA.with(|arena| arena.borrow().location.clone());
    let location = call.alloc_str(&location);
    call.ret_ref(0, location);
    ExternResult::Ok
}

fn queue_navigation(request: NavigationRequest) -> Result<(), &'static str> {
    BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if arena.navigation_requests.len() >= MAX_NAVIGATION_REQUESTS {
            return Err("UI navigation request limit exceeded");
        }
        match &request {
            NavigationRequest::Push(path) | NavigationRequest::Replace(path) => {
                arena.location = path.clone();
            }
            NavigationRequest::Back | NavigationRequest::Forward => {}
        }
        arena.navigation_requests.push(request);
        Ok(())
    })
}

fn location_request(call: &mut ExternCallContext<'_>, replace: bool) -> ExternResult {
    let path = call.arg_str(0).to_string();
    if !valid_location(&path) {
        return ExternResult::Panic("UI navigation path is invalid".to_string());
    }
    let request = if replace {
        NavigationRequest::Replace(path)
    } else {
        NavigationRequest::Push(path)
    };
    match queue_navigation(request) {
        Ok(()) => ExternResult::Ok,
        Err(message) => ExternResult::Panic(message.to_string()),
    }
}

fn navigate(call: &mut ExternCallContext<'_>) -> ExternResult {
    location_request(call, false)
}

fn replace_location(call: &mut ExternCallContext<'_>) -> ExternResult {
    location_request(call, true)
}

fn navigate_back(_call: &mut ExternCallContext<'_>) -> ExternResult {
    match queue_navigation(NavigationRequest::Back) {
        Ok(()) => ExternResult::Ok,
        Err(message) => ExternResult::Panic(message.to_string()),
    }
}

fn navigate_forward(_call: &mut ExternCallContext<'_>) -> ExternResult {
    match queue_navigation(NavigationRequest::Forward) {
        Ok(()) => ExternResult::Ok,
        Err(message) => ExternResult::Panic(message.to_string()),
    }
}

pub fn set_location_from_host(path: &str, invalidate: bool) -> Result<bool, &'static str> {
    if !valid_location(path) {
        return Err("UI host location is invalid");
    }
    BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if arena.location == path {
            return Ok(false);
        }
        arena.location.clear();
        arena.location.push_str(path);
        arena.invalidation_pending |= invalidate;
        Ok(true)
    })
}

pub fn take_navigation_requests() -> Vec<NavigationRequest> {
    BUILD_ARENA.with(|arena| core::mem::take(&mut arena.borrow_mut().navigation_requests))
}

/// Drains the bounded VUS1 request queue emitted by UI goroutines. A platform
/// host pairs each frame with the `ui-system` replay waiter carrying the same
/// request identity, executes it on the platform owner thread, then wakes that
/// exact waiter with a VUS1 response frame.
pub fn take_system_requests() -> Vec<PendingSystemRequest> {
    BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        arena.system_request_bytes = 0;
        arena.system_requests.drain(..).collect()
    })
}

fn resume_system_response(
    call: &mut ExternCallContext<'_>,
) -> Result<Option<SystemResponseEnvelope>, String> {
    let Some(token) = call.take_resume_host_event_token() else {
        return Ok(None);
    };
    let bytes = call
        .take_resume_host_event_data()
        .ok_or_else(|| "UI system request resumed without a response frame".to_string())?;
    let response = decode_system_response(&bytes, SystemLimits::default())
        .map_err(|error| format!("invalid UI system response frame: {error:?}"))?;
    if response.request_id != token {
        return Err(format!(
            "UI system response identity {} does not match replay token {token}",
            response.request_id
        ));
    }
    Ok(Some(response))
}

fn suspend_system_request(
    call: &mut ExternCallContext<'_>,
    request: SystemRequest,
) -> ExternResult {
    let Some(token) = call.try_next_host_event_token() else {
        return ExternResult::Panic("UI system request identity space is exhausted".to_string());
    };
    let frame = match encode_system_request(
        &SystemRequestEnvelope {
            request_id: token,
            request,
        },
        SystemLimits::default(),
    ) {
        Ok(frame) => frame,
        Err(error) => {
            return ExternResult::Panic(format!("invalid UI system request: {error:?}"));
        }
    };
    let queued = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if arena.system_requests.len() >= MAX_SYSTEM_REQUESTS {
            return Err("UI system request count limit exceeded");
        }
        let bytes = arena
            .system_request_bytes
            .checked_add(frame.len())
            .filter(|bytes| *bytes <= MAX_SYSTEM_REQUEST_BYTES)
            .ok_or("UI system request byte limit exceeded")?;
        arena.system_request_bytes = bytes;
        arena.system_requests.push_back(PendingSystemRequest {
            request_id: token,
            frame,
        });
        Ok(())
    });
    if let Err(message) = queued {
        return ExternResult::Panic(message.to_string());
    }
    ExternResult::HostEventWaitAndReplay {
        token,
        source: HostEventReplaySource::UiSystem,
    }
}

fn system_failure_message(failure: &SystemFailure) -> String {
    let kind = match failure.kind {
        vo_ui_system::SystemFailureKind::Denied => "denied",
        vo_ui_system::SystemFailureKind::Unsupported => "unsupported",
        vo_ui_system::SystemFailureKind::Cancelled => "cancelled",
        vo_ui_system::SystemFailureKind::Failed => "failed",
    };
    if failure.message.is_empty() {
        format!("UI system request {kind}")
    } else {
        format!("UI system request {kind}: {}", failure.message)
    }
}

fn clipboard_format(value: i64) -> Result<ClipboardFormat, &'static str> {
    match value {
        1 => Ok(ClipboardFormat::Text),
        2 => Ok(ClipboardFormat::Html),
        3 => Ok(ClipboardFormat::Rgba8),
        _ => Err("UI clipboard format is invalid"),
    }
}

fn runtime_read_clipboard(call: &mut ExternCallContext<'_>) -> ExternResult {
    match resume_system_response(call) {
        Ok(Some(response)) => return return_clipboard_response(call, response.response),
        Ok(None) => {}
        Err(message) => return ExternResult::Panic(message),
    }
    let format = match clipboard_format(call.arg_i64(0)) {
        Ok(format) => format,
        Err(message) => return ExternResult::Panic(message.to_string()),
    };
    suspend_system_request(call, SystemRequest::ReadClipboard(format))
}

fn return_clipboard_response(
    call: &mut ExternCallContext<'_>,
    response: SystemResponse,
) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    call.ret_str(2, "");
    call.ret_i64(3, 0);
    call.ret_i64(4, 0);
    call.ret_nil(5);
    call.ret_bool(6, false);
    match response {
        SystemResponse::Clipboard(None) => call.ret_nil_error(7),
        SystemResponse::Clipboard(Some(ClipboardContent::Text(text))) => {
            call.ret_i64(0, 1);
            call.ret_str(1, &text);
            call.ret_bool(6, true);
            call.ret_nil_error(7);
        }
        SystemResponse::Clipboard(Some(ClipboardContent::Html { html, plain_text })) => {
            call.ret_i64(0, 2);
            call.ret_str(1, &html);
            call.ret_str(2, &plain_text);
            call.ret_bool(6, true);
            call.ret_nil_error(7);
        }
        SystemResponse::Clipboard(Some(ClipboardContent::Rgba8(image))) => {
            call.ret_i64(0, 3);
            call.ret_i64(3, i64::from(image.width));
            call.ret_i64(4, i64::from(image.height));
            call.ret_bytes(5, &image.pixels);
            call.ret_bool(6, true);
            call.ret_nil_error(7);
        }
        SystemResponse::Failure(failure) => {
            call.ret_error_msg(7, &system_failure_message(&failure));
        }
        _ => {
            return ExternResult::Panic(
                "UI clipboard read received an incompatible system response".to_string(),
            );
        }
    }
    ExternResult::Ok
}

fn runtime_write_clipboard(call: &mut ExternCallContext<'_>) -> ExternResult {
    match resume_system_response(call) {
        Ok(Some(response)) => {
            match response.response {
                SystemResponse::Complete => call.ret_nil_error(0),
                SystemResponse::Failure(failure) => {
                    call.ret_error_msg(0, &system_failure_message(&failure));
                }
                _ => {
                    return ExternResult::Panic(
                        "UI clipboard write received an incompatible system response".to_string(),
                    );
                }
            }
            return ExternResult::Ok;
        }
        Ok(None) => {}
        Err(message) => return ExternResult::Panic(message),
    }
    let content = match call.arg_i64(0) {
        1 => ClipboardContent::Text(call.arg_str(1).to_string()),
        2 => ClipboardContent::Html {
            html: call.arg_str(1).to_string(),
            plain_text: call.arg_str(2).to_string(),
        },
        3 => {
            let width = match u32::try_from(call.arg_i64(3)) {
                Ok(value) => value,
                Err(_) => {
                    return ExternResult::Panic("UI clipboard image width is invalid".to_string());
                }
            };
            let height = match u32::try_from(call.arg_i64(4)) {
                Ok(value) => value,
                Err(_) => {
                    return ExternResult::Panic("UI clipboard image height is invalid".to_string());
                }
            };
            ClipboardContent::Rgba8(ClipboardImage {
                width,
                height,
                pixels: call.arg_bytes_owned(5),
            })
        }
        _ => return ExternResult::Panic("UI clipboard content kind is invalid".to_string()),
    };
    suspend_system_request(call, SystemRequest::WriteClipboard(content))
}

fn vo_strings(call: &ExternCallContext<'_>, slot: u16) -> Result<Vec<String>, String> {
    let value = call.arg_ref(slot);
    if value.is_null() {
        return Ok(Vec::new());
    }
    let slice = unsafe { VoSlice::<VoStringElem>::from_ref(value) };
    slice
        .cursor()
        .map(|(index, value)| {
            String::from_utf8(value)
                .map_err(|_| format!("UI string slice element {index} contains invalid UTF-8"))
        })
        .collect()
}

fn vo_i64s(call: &ExternCallContext<'_>, slot: u16) -> Vec<i64> {
    let value = call.arg_ref(slot);
    if value.is_null() {
        return Vec::new();
    }
    let slice = unsafe { VoSlice::<i64>::from_ref(value) };
    slice.cursor().map(|(_, value)| value).collect()
}

fn vo_u64s(call: &ExternCallContext<'_>, slot: u16) -> Vec<u64> {
    let value = call.arg_ref(slot);
    if value.is_null() {
        return Vec::new();
    }
    let slice = unsafe { VoSlice::<u64>::from_ref(value) };
    slice.cursor().map(|(_, value)| value).collect()
}

fn runtime_file_dialog(call: &mut ExternCallContext<'_>) -> ExternResult {
    match resume_system_response(call) {
        Ok(Some(response)) => {
            match response.response {
                SystemResponse::FileDialog(result) => {
                    call.ret_string_slice(0, &result.paths);
                    call.ret_nil_error(1);
                }
                SystemResponse::Failure(failure) => {
                    call.ret_nil(0);
                    call.ret_error_msg(1, &system_failure_message(&failure));
                }
                _ => {
                    return ExternResult::Panic(
                        "UI file dialog received an incompatible system response".to_string(),
                    );
                }
            }
            return ExternResult::Ok;
        }
        Ok(None) => {}
        Err(message) => return ExternResult::Panic(message),
    }
    let kind = match call.arg_i64(0) {
        1 => FileDialogKind::OpenFile,
        2 => FileDialogKind::OpenFiles,
        3 => FileDialogKind::OpenFolder,
        4 => FileDialogKind::OpenFolders,
        5 => FileDialogKind::SaveFile,
        _ => return ExternResult::Panic("UI file dialog kind is invalid".to_string()),
    };
    let names = match vo_strings(call, 5) {
        Ok(values) => values,
        Err(message) => return ExternResult::Panic(message),
    };
    let extensions = match vo_strings(call, 6) {
        Ok(values) => values,
        Err(message) => return ExternResult::Panic(message),
    };
    let counts = vo_i64s(call, 7);
    if names.len() != counts.len() {
        return ExternResult::Panic("UI file dialog filter arrays disagree".to_string());
    }
    let mut extension_cursor = 0_usize;
    let mut filters = Vec::with_capacity(names.len());
    for (name, count) in names.into_iter().zip(counts) {
        let count = match usize::try_from(count) {
            Ok(value) => value,
            Err(_) => {
                return ExternResult::Panic(
                    "UI file dialog extension count is invalid".to_string(),
                );
            }
        };
        let end = match extension_cursor.checked_add(count) {
            Some(end) if end <= extensions.len() => end,
            _ => {
                return ExternResult::Panic("UI file dialog extension arrays disagree".to_string());
            }
        };
        filters.push(FileDialogFilter {
            name,
            extensions: extensions[extension_cursor..end].to_vec(),
        });
        extension_cursor = end;
    }
    if extension_cursor != extensions.len() {
        return ExternResult::Panic("UI file dialog extension arrays disagree".to_string());
    }
    let directory = call.arg_str(2).to_string();
    let file_name = call.arg_str(3).to_string();
    suspend_system_request(
        call,
        SystemRequest::ShowFileDialog(FileDialogRequest {
            kind,
            title: call.arg_str(1).to_string(),
            initial_directory: (!directory.is_empty()).then_some(directory),
            initial_file_name: (!file_name.is_empty()).then_some(file_name),
            filters,
            can_create_directories: call.arg_bool(4),
        }),
    )
}

fn runtime_message_dialog(call: &mut ExternCallContext<'_>) -> ExternResult {
    match resume_system_response(call) {
        Ok(Some(response)) => {
            match response.response {
                SystemResponse::MessageDialog(result) => {
                    call.ret_i64(
                        0,
                        match result {
                            vo_ui_system::MessageDialogResult::Ok => 1,
                            vo_ui_system::MessageDialogResult::Cancel => 2,
                            vo_ui_system::MessageDialogResult::Yes => 3,
                            vo_ui_system::MessageDialogResult::No => 4,
                        },
                    );
                    call.ret_nil_error(1);
                }
                SystemResponse::Failure(failure) => {
                    call.ret_i64(0, 0);
                    call.ret_error_msg(1, &system_failure_message(&failure));
                }
                _ => {
                    return ExternResult::Panic(
                        "UI message dialog received an incompatible system response".to_string(),
                    );
                }
            }
            return ExternResult::Ok;
        }
        Ok(None) => {}
        Err(message) => return ExternResult::Panic(message),
    }
    let level = match call.arg_i64(0) {
        1 => MessageDialogLevel::Info,
        2 => MessageDialogLevel::Warning,
        3 => MessageDialogLevel::Error,
        _ => return ExternResult::Panic("UI message dialog level is invalid".to_string()),
    };
    let buttons = match call.arg_i64(1) {
        1 => MessageDialogButtons::Ok,
        2 => MessageDialogButtons::OkCancel,
        3 => MessageDialogButtons::YesNo,
        4 => MessageDialogButtons::YesNoCancel,
        _ => return ExternResult::Panic("UI message dialog buttons are invalid".to_string()),
    };
    suspend_system_request(
        call,
        SystemRequest::ShowMessageDialog(MessageDialogRequest {
            level,
            buttons,
            title: call.arg_str(2).to_string(),
            description: call.arg_str(3).to_string(),
        }),
    )
}

fn runtime_install_menu(call: &mut ExternCallContext<'_>) -> ExternResult {
    let revision = call.arg_i64(0);
    match resume_system_response(call) {
        Ok(Some(response)) => {
            match response.response {
                SystemResponse::MenuInstalled {
                    revision: installed,
                } if u64::try_from(revision) == Ok(installed) => call.ret_nil_error(0),
                SystemResponse::Failure(failure) => {
                    call.ret_error_msg(0, &system_failure_message(&failure));
                }
                _ => {
                    return ExternResult::Panic(
                        "UI menu install received an incompatible system response".to_string(),
                    );
                }
            }
            return ExternResult::Ok;
        }
        Ok(None) => {}
        Err(message) => return ExternResult::Panic(message),
    }
    let revision = match u64::try_from(revision) {
        Ok(value) if value != 0 => value,
        _ => return ExternResult::Panic("UI menu revision is invalid".to_string()),
    };
    let kinds = vo_i64s(call, 1);
    let ids = vo_u64s(call, 2);
    let parents = vo_i64s(call, 3);
    let labels = match vo_strings(call, 4) {
        Ok(values) => values,
        Err(message) => return ExternResult::Panic(message),
    };
    let flags = vo_i64s(call, 5);
    let shortcuts = match vo_strings(call, 6) {
        Ok(values) => values,
        Err(message) => return ExternResult::Panic(message),
    };
    let count = kinds.len();
    if [
        ids.len(),
        parents.len(),
        labels.len(),
        flags.len(),
        shortcuts.len(),
    ]
    .into_iter()
    .any(|len| len != count)
    {
        return ExternResult::Panic("UI menu arrays disagree".to_string());
    }
    let mut flat = Vec::with_capacity(count);
    for index in 0..count {
        let parent = if parents[index] < 0 {
            None
        } else {
            match usize::try_from(parents[index]) {
                Ok(parent) if parent < index && kinds[parent] == 3 => Some(parent),
                _ => return ExternResult::Panic("UI menu parent is invalid".to_string()),
            }
        };
        let packed = ids[index];
        let id = MenuItemId::new(packed as u32, (packed >> 32) as u32);
        let enabled = flags[index] & 1 != 0;
        let checked = flags[index] & 2 != 0;
        if flags[index] & !3 != 0 {
            return ExternResult::Panic("UI menu flags are invalid".to_string());
        }
        let shortcut = (!shortcuts[index].is_empty()).then(|| shortcuts[index].clone());
        let node = match kinds[index] {
            1 => MenuNode::Command {
                id,
                label: labels[index].clone(),
                enabled,
                shortcut,
            },
            2 => MenuNode::Check {
                id,
                label: labels[index].clone(),
                enabled,
                checked,
                shortcut,
            },
            3 => MenuNode::Submenu {
                id,
                label: labels[index].clone(),
                enabled,
                children: Vec::new(),
            },
            4 => MenuNode::Separator { id },
            _ => return ExternResult::Panic("UI menu item kind is invalid".to_string()),
        };
        flat.push((parent, Some(node)));
    }
    let mut children = (0..count)
        .map(|_| Vec::new())
        .collect::<Vec<Vec<MenuNode>>>();
    let mut roots = Vec::new();
    for index in (0..count).rev() {
        let mut node = flat[index]
            .1
            .take()
            .expect("UI menu node must be consumed exactly once");
        if let MenuNode::Submenu {
            children: node_children,
            ..
        } = &mut node
        {
            children[index].reverse();
            *node_children = core::mem::take(&mut children[index]);
        } else if !children[index].is_empty() {
            return ExternResult::Panic("UI menu leaf contains children".to_string());
        }
        if let Some(parent) = flat[index].0 {
            children[parent].push(node);
        } else {
            roots.push(node);
        }
    }
    roots.reverse();
    suspend_system_request(
        call,
        SystemRequest::InstallMenu(MenuModel { revision, roots }),
    )
}

fn runtime_begin_file_drag(call: &mut ExternCallContext<'_>) -> ExternResult {
    match resume_system_response(call) {
        Ok(Some(response)) => {
            match response.response {
                SystemResponse::Complete => call.ret_nil_error(0),
                SystemResponse::Failure(failure) => {
                    call.ret_error_msg(0, &system_failure_message(&failure));
                }
                _ => {
                    return ExternResult::Panic(
                        "UI file drag received an incompatible system response".to_string(),
                    );
                }
            }
            return ExternResult::Ok;
        }
        Ok(None) => {}
        Err(message) => return ExternResult::Panic(message),
    }
    let paths = match vo_strings(call, 0) {
        Ok(paths) => paths,
        Err(message) => return ExternResult::Panic(message),
    };
    let preview = call.arg_str(1).to_string();
    let mode = match call.arg_i64(2) {
        1 => FileDragMode::Copy,
        2 => FileDragMode::Move,
        _ => return ExternResult::Panic("UI file drag mode is invalid".to_string()),
    };
    suspend_system_request(
        call,
        SystemRequest::BeginFileDrag(FileDragRequest {
            paths,
            preview: (!preview.is_empty()).then_some(preview),
            mode,
        }),
    )
}

fn runtime_invoke_host(call: &mut ExternCallContext<'_>) -> ExternResult {
    match resume_system_response(call) {
        Ok(Some(response)) => {
            match response.response {
                SystemResponse::HostPayload(payload) => {
                    call.ret_bytes(0, &payload);
                    call.ret_nil_error(1);
                }
                SystemResponse::Failure(failure) => {
                    call.ret_nil(0);
                    call.ret_error_msg(1, &system_failure_message(&failure));
                }
                _ => {
                    return ExternResult::Panic(
                        "UI host invocation received an incompatible system response".to_string(),
                    )
                }
            }
            return ExternResult::Ok;
        }
        Ok(None) => {}
        Err(message) => return ExternResult::Panic(message),
    }
    suspend_system_request(
        call,
        SystemRequest::InvokeHost(HostInvocation {
            service: call.arg_str(0).to_string(),
            operation: call.arg_str(1).to_string(),
            payload: call.arg_bytes_owned(2),
        }),
    )
}

fn runtime_wait_event(call: &mut ExternCallContext<'_>) -> ExternResult {
    match resume_system_response(call) {
        Ok(Some(response)) => {
            call.ret_i64(0, 0);
            call.ret_u64(1, 0);
            call.ret_u64(2, 0);
            call.ret_i64(3, 0);
            call.ret_f64(4, 0.0);
            call.ret_f64(5, 0.0);
            call.ret_nil(6);
            match response.response {
                SystemResponse::Event(SystemEvent::MenuActivated { sequence, item }) => {
                    call.ret_i64(0, 1);
                    call.ret_u64(1, sequence);
                    call.ret_u64(
                        2,
                        (u64::from(item.generation) << 32) | u64::from(item.index),
                    );
                    call.ret_nil_error(7);
                }
                SystemResponse::Event(SystemEvent::DragDrop(event)) => {
                    call.ret_i64(0, 2);
                    call.ret_u64(1, event.sequence);
                    call.ret_i64(
                        3,
                        match event.phase {
                            DragDropPhase::Entered => 1,
                            DragDropPhase::Moved => 2,
                            DragDropPhase::Left => 3,
                            DragDropPhase::Dropped => 4,
                        },
                    );
                    call.ret_f64(4, event.x);
                    call.ret_f64(5, event.y);
                    call.ret_string_slice(6, &event.paths);
                    call.ret_nil_error(7);
                }
                SystemResponse::Failure(failure) => {
                    call.ret_error_msg(7, &system_failure_message(&failure));
                }
                _ => {
                    return ExternResult::Panic(
                        "UI system event wait received an incompatible response".to_string(),
                    );
                }
            }
            ExternResult::Ok
        }
        Ok(None) => suspend_system_request(call, SystemRequest::WaitEvent),
        Err(message) => ExternResult::Panic(message),
    }
}

fn start_root_render(call: &mut ExternCallContext<'_>, initial: bool) -> ExternResult {
    let closure_ref = call.arg_ref(0);
    if closure_ref.is_null() {
        return ExternResult::Panic("UI Mount received a nil root function".to_string());
    }
    let leases = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if initial && arena.session_epoch == 0 {
            let epoch = match arena.reload_session_epoch.take() {
                Some(epoch) => epoch,
                None => call
                    .try_next_host_event_token()
                    .ok_or_else(|| "UI session identity space is exhausted".to_string())?,
            };
            arena.session_epoch = epoch;
        }
        let leases = core::mem::take(&mut arena.handlers);
        arena.begin_render(initial);
        Ok(leases)
    });
    let leases = match leases {
        Ok(leases) => leases,
        Err(message) => return ExternResult::Panic(message),
    };
    for lease in leases {
        if let Err(error) = call.gc_release_lease(lease) {
            return ExternResult::Panic(format!("failed to release stale UI handler: {error}"));
        }
    }
    ExternResult::CallClosure {
        closure_ref,
        args: Vec::new(),
    }
}

fn finish_root_render(
    call: &mut ExternCallContext<'_>,
    initial: bool,
    result: &[u64],
) -> ExternResult {
    let Some(handle) = result.first().copied() else {
        return ExternResult::Panic("UI root returned no View value".to_string());
    };
    let batch = BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let view = arena.get(handle)?.clone();
        arena.finish_state_declarations()?;
        if initial {
            let epoch = arena.session_epoch;
            let root = NodeId::new(0, 1);
            let renderer = HeadlessRenderer::new(epoch, root, ProtocolLimits::default());
            if let Some(component) = arena.component.clone() {
                let slots = project_template_slots(&component.plan, &view)
                    .map_err(|error| format!("UI template projection failed: {error}"))?;
                let mut runtime = TemplateRuntime::new(renderer, epoch, root);
                let batch = runtime
                    .mount(component.plan, slots)
                    .map_err(|error| format!("UI initial template render failed: {error}"))?;
                arena.runtime = Some(MountedRuntime::Template(Box::new(runtime)));
                let batch = Some(batch);
                arena.profile.record_batch(batch.as_ref());
                Ok(batch)
            } else {
                let mut runtime = UiRuntime::new(renderer, epoch, root);
                let batch = runtime
                    .mount(view)
                    .map_err(|error| format!("UI initial render failed: {error}"))?;
                arena.runtime = Some(MountedRuntime::Generic(Box::new(runtime)));
                let batch = Some(batch);
                arena.profile.record_batch(batch.as_ref());
                Ok(batch)
            }
        } else {
            let component_plan = arena
                .component
                .as_ref()
                .map(|component| component.plan.clone());
            let batch = match arena
                .runtime
                .as_mut()
                .ok_or_else(|| "UI update has no mounted runtime".to_string())?
            {
                MountedRuntime::Generic(runtime) => runtime
                    .update(view)
                    .map(Some)
                    .map_err(|error| format!("UI update failed: {error}")),
                MountedRuntime::Template(runtime) => {
                    let plan = component_plan
                        .ok_or_else(|| "UI template runtime lost its component plan".to_string())?;
                    let slots = project_template_slots(&plan, &view)
                        .map_err(|error| format!("UI template projection failed: {error}"))?;
                    runtime
                        .update_slots(
                            slots
                                .into_iter()
                                .enumerate()
                                .map(|(index, value)| (SlotId::new(index as u32), value)),
                        )
                        .map_err(|error| format!("UI template update failed: {error}"))
                }
                MountedRuntime::Component(_) => {
                    Err("generic root render reached a component forest".to_string())
                }
            }?;
            arena.profile.record_batch(batch.as_ref());
            Ok(batch)
        }
    });
    let batch = match batch {
        Ok(batch) => batch,
        Err(message) => return ExternResult::Panic(message),
    };
    if let Some(batch) = batch {
        let encoded = match encode_batch(&batch, ProtocolLimits::default()) {
            Ok(encoded) => encoded,
            Err(error) => {
                return ExternResult::Panic(format!("UI protocol encoding failed: {error}"));
            }
        };
        call.set_host_output(encoded);
    }
    wait_for_event(call)
}

fn state_error(message: &'static str) -> ExternResult {
    ExternResult::Panic(message.to_string())
}

fn runtime_enter_component(call: &mut ExternCallContext<'_>) -> ExternResult {
    let identity = call.arg_str(0);
    if identity.is_empty() || identity.len() > MAX_KEY_BYTES {
        return ExternResult::Panic(
            "UI component identity must contain 1..=4096 UTF-8 bytes".to_string(),
        );
    }
    let key = if call.arg_bool(2) {
        let key = call.arg_str(3);
        if key.is_empty() || key.len() > MAX_KEY_BYTES {
            return ExternResult::Panic(
                "UI component key must contain 1..=4096 UTF-8 bytes".to_string(),
            );
        }
        Some(Key::Text(key.to_string()))
    } else {
        None
    };
    match BUILD_ARENA.with(|arena| {
        arena
            .borrow_mut()
            .enter_generic_component(identity.to_string(), call.arg_u64(1), key)
    }) {
        Ok(()) => ExternResult::Ok,
        Err(message) => state_error(message),
    }
}

fn runtime_exit_component(_call: &mut ExternCallContext<'_>) -> ExternResult {
    match BUILD_ARENA.with(|arena| arena.borrow_mut().exit_generic_component()) {
        Ok(()) => ExternResult::Ok,
        Err(message) => state_error(message),
    }
}

fn use_string_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    let initial = StateCell::String(call.arg_str(0).to_string());
    match BUILD_ARENA.with(|arena| arena.borrow_mut().use_state(initial)) {
        Ok(handle) => {
            call.ret_u64(0, handle);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn string_state_value(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = BUILD_ARENA.with(|arena| match arena.borrow().state(call.arg_u64(0))? {
        StateCell::String(value) => Ok(value.clone()),
        _ => Err("UI state handle does not contain a string"),
    });
    match value {
        Ok(value) => {
            let value = call.alloc_str(&value);
            call.ret_ref(0, value);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn set_string_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    let handle = call.arg_u64(0);
    let value = call.arg_str(1).to_string();
    match BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let changed = match arena.state_mut(handle)? {
            StateCell::String(state) => {
                let changed = *state != value;
                if changed {
                    *state = value;
                }
                changed
            }
            _ => return Err("UI state handle does not contain a string"),
        };
        if changed {
            arena.mark_state_dirty(handle)?;
        }
        Ok(())
    }) {
        Ok(()) => ExternResult::Ok,
        Err(message) => state_error(message),
    }
}

fn use_bool_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    match BUILD_ARENA.with(|arena| {
        arena
            .borrow_mut()
            .use_state(StateCell::Bool(call.arg_bool(0)))
    }) {
        Ok(handle) => {
            call.ret_u64(0, handle);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn bool_state_value(call: &mut ExternCallContext<'_>) -> ExternResult {
    match BUILD_ARENA.with(|arena| match arena.borrow().state(call.arg_u64(0))? {
        StateCell::Bool(value) => Ok(*value),
        _ => Err("UI state handle does not contain a bool"),
    }) {
        Ok(value) => {
            call.ret_bool(0, value);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn set_bool_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    let handle = call.arg_u64(0);
    let value = call.arg_bool(1);
    match BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let changed = match arena.state_mut(handle)? {
            StateCell::Bool(state) => {
                let changed = *state != value;
                if changed {
                    *state = value;
                }
                changed
            }
            _ => return Err("UI state handle does not contain a bool"),
        };
        if changed {
            arena.mark_state_dirty(handle)?;
        }
        Ok(())
    }) {
        Ok(()) => ExternResult::Ok,
        Err(message) => state_error(message),
    }
}

fn use_int_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    match BUILD_ARENA.with(|arena| {
        arena
            .borrow_mut()
            .use_state(StateCell::Int(call.arg_i64(0)))
    }) {
        Ok(handle) => {
            call.ret_u64(0, handle);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn int_state_value(call: &mut ExternCallContext<'_>) -> ExternResult {
    match BUILD_ARENA.with(|arena| match arena.borrow().state(call.arg_u64(0))? {
        StateCell::Int(value) => Ok(*value),
        _ => Err("UI state handle does not contain an int"),
    }) {
        Ok(value) => {
            call.ret_i64(0, value);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn set_int_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    let handle = call.arg_u64(0);
    let value = call.arg_i64(1);
    match BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let changed = match arena.state_mut(handle)? {
            StateCell::Int(state) => {
                let changed = *state != value;
                if changed {
                    *state = value;
                }
                changed
            }
            _ => return Err("UI state handle does not contain an int"),
        };
        if changed {
            arena.mark_state_dirty(handle)?;
        }
        Ok(())
    }) {
        Ok(()) => ExternResult::Ok,
        Err(message) => state_error(message),
    }
}

fn int_state_alive(call: &mut ExternCallContext<'_>) -> ExternResult {
    let alive = BUILD_ARENA
        .with(|arena| matches!(arena.borrow().state(call.arg_u64(0)), Ok(StateCell::Int(_))));
    call.ret_bool(0, alive);
    ExternResult::Ok
}

fn int_state_committed(call: &mut ExternCallContext<'_>) -> ExternResult {
    let committed = BUILD_ARENA.with(|arena| arena.borrow().int_state_committed(call.arg_u64(0)));
    call.ret_bool(0, committed);
    ExternResult::Ok
}

fn use_float_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    let initial = call.arg_f64(0);
    if !initial.is_finite() {
        return state_error("UI float state requires a finite initial value");
    }
    match BUILD_ARENA.with(|arena| arena.borrow_mut().use_state(StateCell::Float(initial))) {
        Ok(handle) => {
            call.ret_u64(0, handle);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn float_state_value(call: &mut ExternCallContext<'_>) -> ExternResult {
    match BUILD_ARENA.with(|arena| match arena.borrow().state(call.arg_u64(0))? {
        StateCell::Float(value) => Ok(*value),
        _ => Err("UI state handle does not contain a float"),
    }) {
        Ok(value) => {
            call.ret_f64(0, value);
            ExternResult::Ok
        }
        Err(message) => state_error(message),
    }
}

fn set_float_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    let handle = call.arg_u64(0);
    let value = call.arg_f64(1);
    if !value.is_finite() {
        return state_error("UI float state requires a finite value");
    }
    match BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        let changed = match arena.state_mut(handle)? {
            StateCell::Float(state) => {
                let changed = *state != value;
                if changed {
                    *state = value;
                }
                changed
            }
            _ => return Err("UI state handle does not contain a float"),
        };
        if changed {
            arena.mark_state_dirty(handle)?;
        }
        Ok(())
    }) {
        Ok(()) => ExternResult::Ok,
        Err(message) => state_error(message),
    }
}

fn wait_for_event(call: &mut ExternCallContext<'_>) -> ExternResult {
    let Some(token) = call.try_next_host_event_token() else {
        return ExternResult::Panic("UI event identity space is exhausted".to_string());
    };
    BUILD_ARENA.with(|arena| arena.borrow_mut().phase = MountPhase::WaitingEvent { token });
    ExternResult::HostEventWaitAndReplay {
        token,
        source: HostEventReplaySource::GuiEvent,
    }
}

fn dispatch_event(call: &mut ExternCallContext<'_>, bytes: &[u8]) -> ExternResult {
    let envelope = match decode_event(bytes, ProtocolLimits::default()) {
        Ok(event) => event,
        Err(error) => return ExternResult::Panic(format!("invalid UI event payload: {error}")),
    };
    let lease = match BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if envelope.session_epoch != arena.session_epoch {
            return Err("stale UI event session".to_string());
        }
        if envelope.event.sequence <= arena.last_event_sequence {
            return Err("replayed UI event sequence".to_string());
        }
        if !arena.validates_event(&envelope.event) {
            return Err("UI event does not target a live listener".to_string());
        }
        let lease = arena
            .handler_lease(envelope.event.handler)
            .map_err(str::to_string)?;
        arena.last_event_sequence = envelope.event.sequence;
        arena.phase = MountPhase::AwaitingHandler;
        Ok(lease)
    }) {
        Ok(lease) => lease,
        Err(message) => return ExternResult::Panic(message),
    };
    let closure_ref = match call.gc_lease_root(lease) {
        Ok(closure_ref) => closure_ref,
        Err(error) => {
            return ExternResult::Panic(format!("failed to resolve UI handler: {error}"));
        }
    };
    ExternResult::CallClosure {
        closure_ref,
        args: event_args(call, &envelope.event),
    }
}

fn return_event(call: &mut ExternCallContext<'_>, bytes: &[u8]) -> ExternResult {
    let envelope = match decode_event(bytes, ProtocolLimits::default()) {
        Ok(event) => event,
        Err(error) => return ExternResult::Panic(format!("invalid UI event payload: {error}")),
    };
    let handler = match BUILD_ARENA.with(|arena| {
        let mut arena = arena.borrow_mut();
        if envelope.session_epoch != arena.session_epoch {
            return Err("stale UI event session".to_string());
        }
        if envelope.event.sequence <= arena.last_event_sequence {
            return Err("replayed UI event sequence".to_string());
        }
        if !arena.validates_event(&envelope.event) {
            return Err("UI event does not target a live listener".to_string());
        }
        arena.last_event_sequence = envelope.event.sequence;
        if envelope.event.event == EventType::INVALIDATE {
            arena.force_direct_slots = true;
            arena.phase = MountPhase::Idle;
            return Ok(HandlerId::new(u32::MAX, 0));
        }
        let component_handler = match arena.runtime.as_ref() {
            Some(MountedRuntime::Component(runtime)) => {
                runtime.resolve_event_handler(envelope.event.target, envelope.event.handler)
            }
            _ => None,
        };
        if let Some(component_handler) = component_handler {
            if !arena.bundle_handlers.contains_key(&component_handler) {
                return Err("UI component event has no live closure".to_string());
            }
            arena.pending_component_event = Some((component_handler, envelope.event.clone()));
            arena.phase = MountPhase::BundleHandlerReady;
            return Ok(HandlerId::new(u32::MAX, 0));
        }
        arena.phase = MountPhase::Idle;
        Ok(envelope.event.handler)
    }) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    call.ret_u64(0, u64::from(handler.index()));
    write_event_returns(call, 1, &envelope.event);
    ExternResult::Ok
}

fn write_event_returns(call: &mut ExternCallContext<'_>, base: u16, event: &UiEvent) {
    let values = event_args(call, event);
    for (index, value) in values.into_iter().enumerate() {
        let slot = base + index as u16;
        match index {
            2 | 4 | 7 => call.ret_ref(slot, value as GcRef),
            5 | 6 | 15 | 16 => call.ret_f64(slot, f64::from_bits(value)),
            3 | 9 | 10 => call.ret_bool(slot, value != 0),
            _ => call.ret_u64(slot, value),
        }
    }
}

fn event_args(call: &mut ExternCallContext<'_>, event: &UiEvent) -> Vec<u64> {
    let mut text = call.alloc_str("");
    let mut checked = false;
    let mut key = call.alloc_str("");
    let mut code = call.alloc_str("");
    let mut modifiers = 0_u8;
    let mut repeat = false;
    let mut composing = false;
    let mut x = 0_f64;
    let mut y = 0_f64;
    let mut button = 0_i16;
    let mut buttons = 0_u16;
    let mut pointer_id = 0_i64;
    let mut pointer_kind = 0_u8;
    let mut delta_x = 0_f64;
    let mut delta_y = 0_f64;
    let mut scroll_unit = 0_u8;
    let mut selection_start_utf16 = 0_u32;
    let mut selection_length_utf16 = 0_u32;
    match &event.payload {
        EventPayload::Text(value) => text = call.alloc_str(value),
        EventPayload::Bytes(value) => text = call.alloc_string_bytes(value),
        EventPayload::Toggle(value) => checked = *value,
        EventPayload::Scalar(value) => y = *value as f64,
        EventPayload::Key(value) => {
            key = call.alloc_str(&value.key);
            code = call.alloc_str(&value.code);
            modifiers = u8::from(value.modifiers.shift)
                | (u8::from(value.modifiers.control) << 1)
                | (u8::from(value.modifiers.alt) << 2)
                | (u8::from(value.modifiers.meta) << 3);
            repeat = value.repeat;
            composing = value.composing;
        }
        EventPayload::Pointer(value) => {
            x = value.x;
            y = value.y;
            button = value.button;
            buttons = value.buttons;
            pointer_id = value.pointer_id;
            pointer_kind = value.kind as u8;
            modifiers = u8::from(value.modifiers.shift)
                | (u8::from(value.modifiers.control) << 1)
                | (u8::from(value.modifiers.alt) << 2)
                | (u8::from(value.modifiers.meta) << 3);
        }
        EventPayload::Scroll(value) => {
            x = value.x;
            y = value.y;
            delta_x = value.delta_x;
            delta_y = value.delta_y;
            scroll_unit = value.unit as u8;
            modifiers = u8::from(value.modifiers.shift)
                | (u8::from(value.modifiers.control) << 1)
                | (u8::from(value.modifiers.alt) << 2)
                | (u8::from(value.modifiers.meta) << 3);
        }
        EventPayload::Composition(value) => {
            text = call.alloc_str(&value.text);
            selection_start_utf16 = value.selection_start_utf16;
            selection_length_utf16 = value.selection_length_utf16;
        }
        EventPayload::TextInput(value) => {
            text = call.alloc_str(&value.text);
            selection_start_utf16 = value.selection_start_utf16;
            selection_length_utf16 = value.selection_length_utf16;
        }
        EventPayload::None => {}
    }
    vec![
        u64::from(event.event.0),
        event.sequence,
        text as u64,
        u64::from(checked),
        key as u64,
        x.to_bits(),
        y.to_bits(),
        code as u64,
        u64::from(modifiers),
        u64::from(repeat),
        u64::from(composing),
        i64::from(button) as u64,
        u64::from(buttons),
        pointer_id as u64,
        u64::from(pointer_kind),
        delta_x.to_bits(),
        delta_y.to_bits(),
        u64::from(scroll_unit),
        u64::from(selection_start_utf16),
        u64::from(selection_length_utf16),
    ]
}

fn finish_view(call: &mut ExternCallContext<'_>, view: View) -> ExternResult {
    match BUILD_ARENA.with(|arena| arena.borrow_mut().insert(view)) {
        Ok(handle) => {
            call.ret_u64(0, handle);
            ExternResult::Ok
        }
        Err(message) => ExternResult::Panic(message.to_string()),
    }
}

fn view_from_handle(handle: u64) -> Result<View, &'static str> {
    BUILD_ARENA.with(|arena| arena.borrow().get(handle).cloned())
}

fn children(call: &ExternCallContext<'_>) -> Result<Vec<View>, &'static str> {
    let slice_ref = call.arg_ref(0);
    if slice_ref.is_null() {
        return Ok(Vec::new());
    }
    // The verified extern layout guarantees that the argument is []View and
    // View has exactly one u64 slot in the public module ABI.
    let slice = unsafe { VoSlice::<u64>::from_ref(slice_ref) };
    slice
        .cursor()
        .map(|(_, handle)| view_from_handle(handle))
        .collect()
}

fn container(call: &mut ExternCallContext<'_>, primitive: Primitive) -> ExternResult {
    match children(call) {
        Ok(children) => finish_view(call, View::element(primitive).children(children)),
        Err(message) => ExternResult::Panic(message.to_string()),
    }
}

fn fragment(call: &mut ExternCallContext<'_>) -> ExternResult {
    container(call, Primitive::Fragment)
}

fn box_view(call: &mut ExternCallContext<'_>) -> ExternResult {
    container(call, Primitive::Box)
}

fn row(call: &mut ExternCallContext<'_>) -> ExternResult {
    container(call, Primitive::Row)
}

fn column(call: &mut ExternCallContext<'_>) -> ExternResult {
    container(call, Primitive::Column)
}

fn stack(call: &mut ExternCallContext<'_>) -> ExternResult {
    container(call, Primitive::Stack)
}

fn grid(call: &mut ExternCallContext<'_>) -> ExternResult {
    container(call, Primitive::Grid)
}

fn scroll(call: &mut ExternCallContext<'_>) -> ExternResult {
    container(call, Primitive::Scroll)
}

fn text_view(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = call.arg_str(0).to_string();
    finish_view(
        call,
        View::element(Primitive::Text).child(View::text(value)),
    )
}

fn image_view(call: &mut ExternCallContext<'_>) -> ExternResult {
    let source = call.arg_str(0).to_string();
    let description = call.arg_str(1).to_string();
    finish_view(
        call,
        View::element(Primitive::Image)
            .property(PropertyId::SOURCE, source)
            .property(PropertyId::ACCESSIBLE_NAME, description),
    )
}

fn canvas_view(call: &mut ExternCallContext<'_>) -> ExternResult {
    let program = call.arg_str(0).to_string();
    let description = call.arg_str(1).to_string();
    finish_view(
        call,
        View::element(Primitive::Canvas)
            .property(PropertyId::ROLE, "img")
            .property(PropertyId::GRAPHICS_PROGRAM, program)
            .property(PropertyId::ACCESSIBLE_NAME, description),
    )
}

fn platform_view(call: &mut ExternCallContext<'_>) -> ExternResult {
    let kind = call.arg_str(0).to_string();
    let state = call.arg_str(1).to_string();
    let description = call.arg_str(2).to_string();
    finish_view(
        call,
        View::element(Primitive::PlatformView)
            .property(PropertyId::ROLE, "group")
            .property(PropertyId::CONTENT_TYPE, kind)
            .property(PropertyId::MEDIA_STATE, state)
            .property(PropertyId::ACCESSIBLE_NAME, description),
    )
}

fn lease_handler(call: &mut ExternCallContext<'_>, slot: u16) -> Result<HandlerId, String> {
    let closure = call.arg_ref(slot);
    if closure.is_null() {
        return Err("UI control received a nil event handler".to_string());
    }
    let lease = call
        .gc_lease(closure)
        .map_err(|error| format!("failed to retain UI event handler: {error}"))?;
    BUILD_ARENA
        .with(|arena| arena.borrow_mut().add_handler(lease))
        .map_err(str::to_string)
}

fn button(call: &mut ExternCallContext<'_>) -> ExternResult {
    let label = call.arg_str(0).to_string();
    let handler = match lease_handler(call, 1) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    finish_view(
        call,
        View::element(Primitive::Button)
            .property(PropertyId::ROLE, "button")
            .property(PropertyId::ACCESSIBLE_NAME, label.clone())
            .listener(Listener::new(EventType::CLICK, handler))
            .child(View::text(label)),
    )
}

fn guest_handler(call: &ExternCallContext<'_>, slot: u16) -> Result<HandlerId, String> {
    let index = u32::try_from(call.arg_u64(slot))
        .map_err(|_| "UI guest handler identity exceeds u32".to_string())?;
    if index as usize >= MAX_BUILD_HANDLERS {
        return Err("UI construction exceeded the per-mount handler limit".to_string());
    }
    let generation = BUILD_ARENA.with(|arena| arena.borrow().handler_generation());
    Ok(HandlerId::new(index, generation))
}

fn button_with_handler(call: &mut ExternCallContext<'_>, handler: HandlerId) -> ExternResult {
    let label = call.arg_str(0).to_string();
    finish_view(
        call,
        View::element(Primitive::Button)
            .property(PropertyId::ROLE, "button")
            .property(PropertyId::ACCESSIBLE_NAME, label.clone())
            .listener(Listener::new(EventType::CLICK, handler))
            .child(View::text(label)),
    )
}

fn runtime_button(call: &mut ExternCallContext<'_>) -> ExternResult {
    match guest_handler(call, 1) {
        Ok(handler) => button_with_handler(call, handler),
        Err(message) => ExternResult::Panic(message),
    }
}

fn text_input(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = call.arg_str(0).to_string();
    let placeholder = call.arg_str(1).to_string();
    let handler = match lease_handler(call, 2) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    finish_view(
        call,
        View::element(Primitive::TextInput)
            .property(PropertyId::ROLE, "textbox")
            .property(PropertyId::VALUE, value)
            .property(PropertyId::PLACEHOLDER, placeholder)
            .listener(Listener::new(EventType::INPUT, handler)),
    )
}

fn text_input_with_handler(call: &mut ExternCallContext<'_>, handler: HandlerId) -> ExternResult {
    let value = call.arg_str(0).to_string();
    let placeholder = call.arg_str(1).to_string();
    finish_view(
        call,
        View::element(Primitive::TextInput)
            .property(PropertyId::ROLE, "textbox")
            .property(PropertyId::VALUE, value)
            .property(PropertyId::PLACEHOLDER, placeholder)
            .listener(Listener::new(EventType::INPUT, handler)),
    )
}

fn runtime_text_input(call: &mut ExternCallContext<'_>) -> ExternResult {
    match guest_handler(call, 2) {
        Ok(handler) => text_input_with_handler(call, handler),
        Err(message) => ExternResult::Panic(message),
    }
}

fn text_area(call: &mut ExternCallContext<'_>) -> ExternResult {
    let handler = match lease_handler(call, 2) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    text_area_with_handler(call, handler)
}

fn text_area_with_handler(call: &mut ExternCallContext<'_>, handler: HandlerId) -> ExternResult {
    let value = call.arg_str(0).to_string();
    let placeholder = call.arg_str(1).to_string();
    finish_view(
        call,
        View::element(Primitive::TextArea)
            .property(PropertyId::ROLE, "textbox")
            .property(PropertyId::VALUE, value)
            .property(PropertyId::PLACEHOLDER, placeholder)
            .listener(Listener::new(EventType::INPUT, handler)),
    )
}

fn runtime_text_area(call: &mut ExternCallContext<'_>) -> ExternResult {
    match guest_handler(call, 2) {
        Ok(handler) => text_area_with_handler(call, handler),
        Err(message) => ExternResult::Panic(message),
    }
}

fn toggle(call: &mut ExternCallContext<'_>) -> ExternResult {
    let checked = call.arg_bool(0);
    let label = call.arg_str(1).to_string();
    let handler = match lease_handler(call, 2) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    finish_view(
        call,
        View::element(Primitive::Toggle)
            .property(PropertyId::ROLE, "switch")
            .property(PropertyId::CHECKED, checked)
            .property(PropertyId::ACCESSIBLE_NAME, label)
            .listener(Listener::new(EventType::CHANGE, handler)),
    )
}

fn toggle_with_handler(call: &mut ExternCallContext<'_>, handler: HandlerId) -> ExternResult {
    let checked = call.arg_bool(0);
    let label = call.arg_str(1).to_string();
    finish_view(
        call,
        View::element(Primitive::Toggle)
            .property(PropertyId::ROLE, "switch")
            .property(PropertyId::CHECKED, checked)
            .property(PropertyId::ACCESSIBLE_NAME, label)
            .listener(Listener::new(EventType::CHANGE, handler)),
    )
}

fn runtime_toggle(call: &mut ExternCallContext<'_>) -> ExternResult {
    match guest_handler(call, 2) {
        Ok(handler) => toggle_with_handler(call, handler),
        Err(message) => ExternResult::Panic(message),
    }
}

fn slider_with_handler(call: &mut ExternCallContext<'_>, handler: HandlerId) -> ExternResult {
    let value = call.arg_f64(0);
    let minimum = call.arg_f64(1);
    let maximum = call.arg_f64(2);
    let step = call.arg_f64(3);
    let label = call.arg_str(4).to_string();
    if !value.is_finite()
        || !minimum.is_finite()
        || !maximum.is_finite()
        || !step.is_finite()
        || maximum <= minimum
        || step <= 0.0
        || value < minimum
        || value > maximum
        || label.is_empty()
    {
        return ExternResult::Panic("UI slider contract is invalid".to_string());
    }
    finish_view(
        call,
        View::element(Primitive::Slider)
            .property(PropertyId::ROLE, "slider")
            .property(PropertyId::ACCESSIBLE_NAME, label)
            .property(PropertyId::VALUE, value)
            .property(PropertyId::MIN_VALUE, minimum)
            .property(PropertyId::MAX_VALUE, maximum)
            .property(PropertyId::STEP_VALUE, step)
            .listener(Listener::new(EventType::INPUT, handler)),
    )
}

fn slider(call: &mut ExternCallContext<'_>) -> ExternResult {
    match lease_handler(call, 5) {
        Ok(handler) => slider_with_handler(call, handler),
        Err(message) => ExternResult::Panic(message),
    }
}

fn runtime_slider(call: &mut ExternCallContext<'_>) -> ExternResult {
    match guest_handler(call, 5) {
        Ok(handler) => slider_with_handler(call, handler),
        Err(message) => ExternResult::Panic(message),
    }
}

fn modify(call: &mut ExternCallContext<'_>, property: PropertyId, value: Value) -> ExternResult {
    match view_from_handle(call.arg_u64(0)) {
        Ok(view) => finish_view(call, view.property(property, value)),
        Err(message) => ExternResult::Panic(message.to_string()),
    }
}

fn width(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::WIDTH, value)
}

fn height(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::HEIGHT, value)
}

fn min_width(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::MIN_WIDTH, value)
}

fn min_height(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::MIN_HEIGHT, value)
}

fn max_width(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::MAX_WIDTH, value)
}

fn max_height(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::MAX_HEIGHT, value)
}

fn flex(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::FLEX, Value::F64(call.arg_f64(1)))
}

fn gap(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::GAP, value)
}

fn padding(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::PADDING, value)
}

fn background(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(
        call,
        PropertyId::BACKGROUND,
        Value::Color(call.arg_u64(1) as u32),
    )
}

fn foreground(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(
        call,
        PropertyId::FOREGROUND,
        Value::Color(call.arg_u64(1) as u32),
    )
}

fn font_size(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::FONT_SIZE, value)
}

fn font_weight(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::FONT_WEIGHT, Value::I64(call.arg_i64(1)))
}

fn text_property(call: &mut ExternCallContext<'_>, property: PropertyId) -> ExternResult {
    let value = Value::Text(call.arg_str(1).to_string());
    modify(call, property, value)
}

fn align(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::ALIGN)
}

fn justify(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::JUSTIFY)
}

fn grid_columns(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::GRID_COLUMNS)
}

fn grid_template_areas(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::GRID_TEMPLATE_AREAS)
}

fn grid_area(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::GRID_AREA)
}

fn overflow(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::OVERFLOW)
}

fn radius(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::RADIUS, value)
}

fn border_color(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(
        call,
        PropertyId::BORDER_COLOR,
        Value::Color(call.arg_u64(1) as u32),
    )
}

fn border_width(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Length(Length::Px(call.arg_f64(1) as f32));
    modify(call, PropertyId::BORDER_WIDTH, value)
}

fn scroll_x(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::SCROLL_X, Value::F64(call.arg_f64(1)))
}

fn scroll_y(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::SCROLL_Y, Value::F64(call.arg_f64(1)))
}

fn disabled(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::DISABLED, Value::Bool(call.arg_bool(1)))
}

fn checked(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::CHECKED, Value::Bool(call.arg_bool(1)))
}

fn role(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::ROLE)
}

fn accessible_name(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Text(call.arg_str(1).to_string());
    modify(call, PropertyId::ACCESSIBLE_NAME, value)
}

fn accessible_description(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::ACCESSIBLE_DESCRIPTION)
}

fn accessible_value(call: &mut ExternCallContext<'_>) -> ExternResult {
    text_property(call, PropertyId::VALUE)
}

fn required(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::REQUIRED, Value::Bool(call.arg_bool(1)))
}

fn invalid(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::INVALID, Value::Bool(call.arg_bool(1)))
}

fn selected(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::SELECTED, Value::Bool(call.arg_bool(1)))
}

fn expanded(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::EXPANDED, Value::Bool(call.arg_bool(1)))
}

fn pressed(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::PRESSED, Value::Bool(call.arg_bool(1)))
}

fn current(call: &mut ExternCallContext<'_>) -> ExternResult {
    match call.arg_str(1) {
        "false" | "true" | "page" | "step" | "location" | "date" | "time" => {
            text_property(call, PropertyId::CURRENT)
        }
        _ => ExternResult::Panic("UI current token is invalid".to_string()),
    }
}

fn hidden(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::HIDDEN, Value::Bool(call.arg_bool(1)))
}

fn accessibility_hidden(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(
        call,
        PropertyId::ACCESSIBILITY_HIDDEN,
        Value::Bool(call.arg_bool(1)),
    )
}

fn focusable(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::FOCUSABLE, Value::Bool(call.arg_bool(1)))
}

fn bounded_text_property(
    call: &mut ExternCallContext<'_>,
    property: PropertyId,
    maximum_bytes: usize,
    label: &str,
) -> ExternResult {
    if call.arg_str(1).len() > maximum_bytes {
        return ExternResult::Panic(format!("UI {label} exceeds its byte limit"));
    }
    text_property(call, property)
}

fn source(call: &mut ExternCallContext<'_>) -> ExternResult {
    bounded_text_property(call, PropertyId::SOURCE, 4_096, "source")
}

fn content_type(call: &mut ExternCallContext<'_>) -> ExternResult {
    bounded_text_property(call, PropertyId::CONTENT_TYPE, 255, "content type")
}

fn fit(call: &mut ExternCallContext<'_>) -> ExternResult {
    match call.arg_str(1) {
        "contain" | "cover" | "fill" | "none" | "scale-down" => {
            text_property(call, PropertyId::FIT)
        }
        _ => ExternResult::Panic("UI asset fit is invalid".to_string()),
    }
}

fn opacity(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = call.arg_f64(1);
    if !value.is_finite() || !(0.0..=1.0).contains(&value) {
        return ExternResult::Panic(
            "UI opacity must be finite and between zero and one".to_string(),
        );
    }
    modify(call, PropertyId::OPACITY, Value::F64(value))
}

fn transform(call: &mut ExternCallContext<'_>) -> ExternResult {
    bounded_text_property(call, PropertyId::TRANSFORM, 512, "transform")
}

fn graphics_program(call: &mut ExternCallContext<'_>) -> ExternResult {
    bounded_text_property(
        call,
        PropertyId::GRAPHICS_PROGRAM,
        1_048_576,
        "graphics program",
    )
}

fn media_state(call: &mut ExternCallContext<'_>) -> ExternResult {
    bounded_text_property(call, PropertyId::MEDIA_STATE, 65_536, "media state")
}

fn poster(call: &mut ExternCallContext<'_>) -> ExternResult {
    bounded_text_property(call, PropertyId::POSTER, 4_096, "poster")
}

fn modal(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::MODAL, Value::Bool(call.arg_bool(1)))
}

fn auto_focus(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(call, PropertyId::AUTO_FOCUS, Value::Bool(call.arg_bool(1)))
}

fn pointer_events(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = call.arg_str(1);
    if value != "auto" && value != "none" {
        return ExternResult::Panic("UI pointer events must be auto or none".to_string());
    }
    text_property(call, PropertyId::POINTER_EVENTS)
}

fn capture_pointer(call: &mut ExternCallContext<'_>) -> ExternResult {
    modify(
        call,
        PropertyId::POINTER_CAPTURE,
        Value::Bool(call.arg_bool(1)),
    )
}

fn flow_direction(call: &mut ExternCallContext<'_>) -> ExternResult {
    let direction = call.arg_i64(1);
    if direction != 0 && direction != 1 {
        return ExternResult::Panic("UI flow direction is invalid".to_string());
    }
    modify(call, PropertyId::FLOW_DIRECTION, Value::I64(direction))
}

fn portal(call: &mut ExternCallContext<'_>) -> ExternResult {
    let layer = call.arg_i64(1);
    if !(-1_000_000..=1_000_000).contains(&layer) {
        return ExternResult::Panic("UI portal layer exceeds the portable range".to_string());
    }
    modify(call, PropertyId::PORTAL_LAYER, Value::I64(layer))
}

fn focus_request(call: &mut ExternCallContext<'_>) -> ExternResult {
    let token = call.arg_i64(1);
    if token < 0 {
        return ExternResult::Panic("UI focus request token cannot be negative".to_string());
    }
    modify(call, PropertyId::FOCUS_REQUEST, Value::I64(token))
}

fn key_view(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = call.arg_str(1);
    if value.is_empty() || value.len() > MAX_KEY_BYTES {
        return ExternResult::Panic("UI key must contain 1..=4096 UTF-8 bytes".to_string());
    }
    match view_from_handle(call.arg_u64(0)) {
        Ok(view) => finish_view(call, view.key(Key::Text(value.to_string()))),
        Err(message) => ExternResult::Panic(message.to_string()),
    }
}

fn selection_start_utf16(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = call.arg_i64(1);
    if value < 0 {
        return ExternResult::Panic("UI selection start cannot be negative".to_string());
    }
    modify(call, PropertyId::SELECTION_START_UTF16, Value::I64(value))
}

fn selection_length_utf16(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = call.arg_i64(1);
    if value < 0 {
        return ExternResult::Panic("UI selection length cannot be negative".to_string());
    }
    modify(call, PropertyId::SELECTION_LENGTH_UTF16, Value::I64(value))
}

fn test_id(call: &mut ExternCallContext<'_>) -> ExternResult {
    let value = Value::Text(call.arg_str(1).to_string());
    modify(call, PropertyId::TEST_ID, value)
}

fn listen(call: &mut ExternCallContext<'_>, event: EventType) -> ExternResult {
    let view = match view_from_handle(call.arg_u64(0)) {
        Ok(view) => view,
        Err(message) => return ExternResult::Panic(message.to_string()),
    };
    let handler = match lease_handler(call, 1) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    finish_view(call, view.listener(Listener::new(event, handler)))
}

fn runtime_listen(call: &mut ExternCallContext<'_>, event: EventType) -> ExternResult {
    let view = match view_from_handle(call.arg_u64(0)) {
        Ok(view) => view,
        Err(message) => return ExternResult::Panic(message.to_string()),
    };
    let handler = match guest_handler(call, 1) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    finish_view(call, view.listener(Listener::new(event, handler)))
}

fn listen_capture(call: &mut ExternCallContext<'_>, event: EventType) -> ExternResult {
    let view = match view_from_handle(call.arg_u64(0)) {
        Ok(view) => view,
        Err(message) => return ExternResult::Panic(message.to_string()),
    };
    let handler = match lease_handler(call, 1) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    finish_view(
        call,
        view.listener(Listener::new(event, handler).with_options(ListenerOptions {
            capture: true,
            passive: false,
            once: false,
        })),
    )
}

fn runtime_listen_capture(call: &mut ExternCallContext<'_>, event: EventType) -> ExternResult {
    let view = match view_from_handle(call.arg_u64(0)) {
        Ok(view) => view,
        Err(message) => return ExternResult::Panic(message.to_string()),
    };
    let handler = match guest_handler(call, 1) {
        Ok(handler) => handler,
        Err(message) => return ExternResult::Panic(message),
    };
    finish_view(
        call,
        view.listener(Listener::new(event, handler).with_options(ListenerOptions {
            capture: true,
            passive: false,
            once: false,
        })),
    )
}

fn on_click(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::CLICK)
}

fn runtime_on_click(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::CLICK)
}

fn on_submit(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::SUBMIT)
}

fn runtime_on_submit(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::SUBMIT)
}

fn on_focus(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::FOCUS)
}

fn runtime_on_focus(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::FOCUS)
}

fn on_blur(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::BLUR)
}

fn runtime_on_blur(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::BLUR)
}

fn on_key_down(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::KEY_DOWN)
}

fn runtime_on_key_down(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::KEY_DOWN)
}

fn on_key_down_capture(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen_capture(call, EventType::KEY_DOWN)
}

fn runtime_on_key_down_capture(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen_capture(call, EventType::KEY_DOWN)
}

fn on_key_up(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::KEY_UP)
}

fn runtime_on_key_up(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::KEY_UP)
}

fn on_pointer_down(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::POINTER_DOWN)
}

fn runtime_on_pointer_down(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::POINTER_DOWN)
}

fn on_pointer_move(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::POINTER_MOVE)
}

fn runtime_on_pointer_move(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::POINTER_MOVE)
}

fn on_pointer_up(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::POINTER_UP)
}

fn runtime_on_pointer_up(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::POINTER_UP)
}

fn on_pointer_cancel(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::POINTER_CANCEL)
}

fn runtime_on_pointer_cancel(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::POINTER_CANCEL)
}

fn on_scroll(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::SCROLL)
}

fn runtime_on_scroll(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::SCROLL)
}

fn on_composition_start(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::COMPOSITION_START)
}

fn runtime_on_composition_start(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::COMPOSITION_START)
}

fn on_composition_update(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::COMPOSITION_UPDATE)
}

fn runtime_on_composition_update(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::COMPOSITION_UPDATE)
}

fn on_composition_end(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::COMPOSITION_END)
}

fn runtime_on_composition_end(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::COMPOSITION_END)
}

fn on_selection_change(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::SELECTION_CHANGE)
}

fn runtime_on_selection_change(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::SELECTION_CHANGE)
}

fn on_wheel(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::WHEEL)
}

fn runtime_on_wheel(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::WHEEL)
}

fn on_layout(call: &mut ExternCallContext<'_>) -> ExternResult {
    listen(call, EventType::LAYOUT)
}

fn runtime_on_layout(call: &mut ExternCallContext<'_>) -> ExternResult {
    runtime_listen(call, EventType::LAYOUT)
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common_core::bytecode::{ParamShape, ReturnShape};
    use vo_common_core::extern_key::ExternKeyRef;
    use vo_ui_artifact::{HandlerArtifact, SlotArtifact, StateArtifact};
    use vo_ui_plan::{ComponentPlan, LocalNodeId, PlanLimits, TemplateNode};

    fn extern_def(function: &str) -> ExternDef {
        let name = ExternKeyRef::new(UI_MODULE_PATH, function)
            .encode()
            .unwrap();
        ExternDef::new(
            name,
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(0),
            ExternEffects::UNKNOWN_CONTROL,
            Vec::new(),
        )
    }

    fn component(states: &[(&str, u64)]) -> ComponentArtifact {
        let mut plan = ComponentPlan::new(LocalNodeId::new(0));
        plan.nodes
            .push(TemplateNode::element(LocalNodeId::new(0), Primitive::Box));
        ComponentArtifact {
            identity: "local/reload::App".to_string(),
            component_name: "App".to_string(),
            mode: ExecutionMode::RootFallback,
            plan: plan.validate(PlanLimits::default()).unwrap(),
            states: states
                .iter()
                .map(|(key, type_fingerprint)| StateArtifact {
                    key: (*key).to_string(),
                    type_fingerprint: *type_fingerprint,
                    has_initializer: true,
                    initializer_func: None,
                    dependent_slots: Vec::new(),
                    captured_by_handlers: Vec::new(),
                })
                .collect(),
            slots: Vec::<SlotArtifact>::new(),
            handlers: Vec::<HandlerArtifact>::new(),
        }
    }

    #[test]
    fn direct_slider_properties_keep_their_numeric_type() {
        assert_eq!(
            direct_property_value(
                PropertyId::VALUE,
                Some(Primitive::Slider),
                85.0_f64.to_bits(),
            )
            .unwrap(),
            Value::F64(85.0),
        );
        assert_eq!(
            direct_property_value(PropertyId::STEP_VALUE, None, 0.5_f64.to_bits()).unwrap(),
            Value::F64(0.5),
        );
    }

    #[test]
    fn view_handles_are_generation_checked_and_immutable() {
        let mut arena = BuildArena::default();
        arena.begin_render(true);
        let first = arena.insert(View::text("first")).unwrap();
        assert_eq!(arena.get(first).unwrap(), &View::text("first"));
        arena.begin_render(true);
        assert_eq!(arena.get(first), Err("stale UI View handle"));
        let second = arena.insert(View::text("second")).unwrap();
        assert_ne!(first, second);
    }

    #[test]
    fn registry_installs_only_requested_official_providers() {
        let externs = vec![
            extern_def("Text"),
            extern_def("Mount"),
            extern_def("runtimeImage"),
            extern_def("runtimeCanvas"),
            extern_def("runtimePlatformView"),
            extern_def("runtimeTextArea"),
            extern_def("Source"),
            extern_def("GraphicsProgram"),
        ];
        let mut registry = ExternRegistry::new();
        register_externs(&mut registry, &externs).unwrap();
        for external in &externs {
            assert!(registry.registered_by_name(&external.name).is_some());
        }
    }

    #[test]
    fn typed_state_cells_initialize_once_and_validate_declaration_order() {
        let mut arena = BuildArena::default();
        arena.begin_render(true);
        let text = arena
            .use_state(StateCell::String("first".to_string()))
            .unwrap();
        let flag = arena.use_state(StateCell::Bool(true)).unwrap();
        arena.finish_state_declarations().unwrap();

        arena.begin_render(false);
        assert_eq!(
            arena
                .use_state(StateCell::String("ignored".to_string()))
                .unwrap(),
            text
        );
        assert_eq!(
            arena.state(text),
            Ok(&StateCell::String("first".to_string()))
        );
        assert_eq!(arena.state(flag), Ok(&StateCell::Bool(true)));
        assert_eq!(
            arena.use_state(StateCell::Int(1)),
            Err("UI state declaration order changed its value type")
        );
    }

    #[test]
    fn component_state_checkpoint_restores_cells_handles_and_dirty_frontier() {
        let mut arena = BuildArena::default();
        let key = BundleStateKey {
            component: ComponentTypeId::new("local/checkpoint", "Counter"),
            path: vec![BundleInstanceIdentity::Key(Key::from("primary"))],
            field: 0,
            type_fingerprint: 7,
        };
        arena.state_cells.push(StateCell::Int(3));
        arena.bundle_state_handles.insert(key.clone(), 1);
        arena.dirty_states.insert(2);
        let checkpoint = BundleStateCheckpoint::capture(&arena);

        arena.state_cells.push(StateCell::Int(99));
        arena.bundle_state_handles.insert(
            BundleStateKey {
                field: 1,
                ..key.clone()
            },
            2,
        );
        arena.dirty_states.insert(9);
        checkpoint.restore(&mut arena);

        assert_eq!(arena.state_cells, vec![StateCell::Int(3)]);
        assert_eq!(arena.bundle_state_handles, BTreeMap::from([(key, 1)]));
        assert_eq!(arena.dirty_states, BTreeSet::from([2]));
    }

    #[test]
    fn structured_work_observes_state_only_after_commit() {
        let mut arena = BuildArena::default();
        arena.begin_render(true);
        let handle = arena.use_state(StateCell::Int(7)).unwrap();
        assert!(!arena.int_state_committed(handle));
        arena.phase = MountPhase::WaitingEvent { token: 1 };
        assert!(arena.int_state_committed(handle));
        arena.begin_render(false);
        assert!(!arena.int_state_committed(handle));
    }

    #[test]
    fn generic_component_scopes_preserve_keyed_state_and_reclaim_disposed_cells() {
        let mut arena = BuildArena::default();
        arena.begin_render(true);
        arena
            .enter_generic_component(
                "local/components::Counter".to_string(),
                1,
                Some(Key::Text("alpha".to_string())),
            )
            .unwrap();
        let alpha = arena.use_state(StateCell::Int(1)).unwrap();
        arena.exit_generic_component().unwrap();
        arena
            .enter_generic_component(
                "local/components::Counter".to_string(),
                1,
                Some(Key::Text("beta".to_string())),
            )
            .unwrap();
        let beta = arena.use_state(StateCell::Int(2)).unwrap();
        arena.exit_generic_component().unwrap();
        arena.finish_state_declarations().unwrap();
        *arena.state_mut(alpha).unwrap() = StateCell::Int(11);

        arena.begin_render(false);
        arena
            .enter_generic_component(
                "local/components::Counter".to_string(),
                1,
                Some(Key::Text("beta".to_string())),
            )
            .unwrap();
        assert_eq!(arena.use_state(StateCell::Int(0)).unwrap(), beta);
        arena.exit_generic_component().unwrap();
        arena
            .enter_generic_component(
                "local/components::Counter".to_string(),
                1,
                Some(Key::Text("alpha".to_string())),
            )
            .unwrap();
        assert_eq!(arena.use_state(StateCell::Int(0)).unwrap(), alpha);
        arena.exit_generic_component().unwrap();
        arena.finish_state_declarations().unwrap();
        assert_eq!(arena.state(alpha), Ok(&StateCell::Int(11)));
        assert_eq!(arena.state(beta), Ok(&StateCell::Int(2)));

        arena.begin_render(false);
        arena
            .enter_generic_component(
                "local/components::Counter".to_string(),
                1,
                Some(Key::Text("beta".to_string())),
            )
            .unwrap();
        assert_eq!(arena.use_state(StateCell::Int(0)).unwrap(), beta);
        arena.exit_generic_component().unwrap();
        arena.finish_state_declarations().unwrap();
        assert_eq!(arena.state(alpha), Err("stale UI component state handle"));
        assert_eq!(arena.state(beta), Ok(&StateCell::Int(2)));

        arena.begin_render(false);
        arena
            .enter_generic_component(
                "local/components::Counter".to_string(),
                1,
                Some(Key::Text("alpha".to_string())),
            )
            .unwrap();
        let replacement_alpha = arena.use_state(StateCell::Int(5)).unwrap();
        arena.exit_generic_component().unwrap();
        arena.finish_state_declarations().unwrap();
        assert_ne!(replacement_alpha, alpha);
        assert_eq!(arena.state(alpha), Err("stale UI component state handle"));
        assert_eq!(arena.state(beta), Err("stale UI component state handle"));
        assert_eq!(arena.state(replacement_alpha), Ok(&StateCell::Int(5)));
    }

    #[test]
    fn worker_invalidation_is_coalesced_and_only_targets_reserved_handler_sentinel() {
        BUILD_ARENA.with(|cell| {
            let root = NodeId::new(0, 1);
            let renderer = HeadlessRenderer::new(7, root, ProtocolLimits::default());
            let mut runtime = UiRuntime::new(renderer, 7, root);
            runtime.mount(View::element(Primitive::Box)).unwrap();
            let mut arena = cell.borrow_mut();
            arena.reset(None);
            arena.runtime = Some(MountedRuntime::Generic(Box::new(runtime)));
            arena.invalidation_pending = true;
            let valid = UiEvent {
                handler: HandlerId::new(u32::MAX, 99),
                event: EventType::INVALIDATE,
                target: root,
                sequence: 1,
                payload: EventPayload::None,
            };
            assert!(arena.validates_event(&valid));
            assert!(!arena.validates_event(&UiEvent {
                handler: HandlerId::new(0, 99),
                ..valid
            }));
        });
        assert!(take_invalidation_request());
        assert!(!take_invalidation_request());
    }

    #[test]
    fn navigation_state_is_bounded_coalesced_and_reload_stable() {
        BUILD_ARENA.with(|cell| cell.borrow_mut().reset(None));
        assert_eq!(set_location_from_host("/initial?tab=1", false), Ok(true));
        assert!(!take_invalidation_request());
        assert_eq!(set_location_from_host("/initial?tab=1", true), Ok(false));
        assert_eq!(
            set_location_from_host("//example.com", true),
            Err("UI host location is invalid")
        );

        queue_navigation(NavigationRequest::Push("/settings#profile".to_string())).unwrap();
        queue_navigation(NavigationRequest::Back).unwrap();
        assert_eq!(
            take_navigation_requests(),
            vec![
                NavigationRequest::Push("/settings#profile".to_string()),
                NavigationRequest::Back,
            ]
        );
        assert!(take_navigation_requests().is_empty());

        let snapshot = BUILD_ARENA.with(|cell| cell.borrow().reload_snapshot());
        let reloaded = BuildArena::for_reload(None, None, &snapshot).unwrap();
        assert_eq!(reloaded.location, "/settings#profile");

        assert_eq!(set_location_from_host("/back", true), Ok(true));
        assert!(take_invalidation_request());
    }

    #[test]
    fn platform_viewport_is_validated_coalesced_and_reload_stable() {
        BUILD_ARENA.with(|cell| cell.borrow_mut().reset(None));
        assert_eq!(set_platform_viewport(800.0, 600.0, 2.0, true), Ok(true));
        assert!(take_invalidation_request());
        assert_eq!(set_platform_viewport(800.0, 600.0, 2.0, true), Ok(false));
        assert!(!take_invalidation_request());
        assert_eq!(
            set_platform_viewport(f64::NAN, 600.0, 2.0, true),
            Err("UI platform viewport metrics are invalid")
        );
        let snapshot = BUILD_ARENA.with(|cell| cell.borrow().reload_snapshot());
        let reloaded = BuildArena::for_reload(None, None, &snapshot).unwrap();
        assert_eq!(reloaded.viewport_width, 800.0);
        assert_eq!(reloaded.viewport_height, 600.0);
        assert_eq!(reloaded.scale_factor, 2.0);
    }

    #[test]
    fn reload_checkpoint_migrates_reordered_state_and_rolls_back_on_drop() {
        BUILD_ARENA.with(|cell| {
            let mut arena = cell.borrow_mut();
            arena.reset(Some(component(&[("name", 1), ("count", 2)])));
            arena.state_cells = vec![StateCell::String("Ada".into()), StateCell::Int(7)];
            arena.phase = MountPhase::WaitingEvent { token: 41 };
            arena.session_epoch = 7;
        });

        {
            let _checkpoint = begin_reload(Some(component(&[("count", 2), ("name", 1)]))).unwrap();
            BUILD_ARENA.with(|cell| {
                let mut arena = cell.borrow_mut();
                assert_eq!(arena.session_epoch, 0);
                assert_eq!(arena.reload_session_epoch, Some(8));
                arena.begin_render(true);
                let count = arena.use_state(StateCell::Int(0)).unwrap();
                let name = arena
                    .use_state(StateCell::String("new".to_string()))
                    .unwrap();
                assert_eq!(arena.state(count), Ok(&StateCell::Int(7)));
                assert_eq!(arena.state(name), Ok(&StateCell::String("Ada".into())));
            });
        }

        BUILD_ARENA.with(|cell| {
            let arena = cell.borrow();
            assert_eq!(arena.phase, MountPhase::WaitingEvent { token: 41 });
            assert_eq!(arena.session_epoch, 7);
            assert_eq!(
                arena.state_cells,
                vec![StateCell::String("Ada".into()), StateCell::Int(7)]
            );
        });
    }

    #[test]
    fn committed_reload_reinitializes_changed_state_types() {
        BUILD_ARENA.with(|cell| {
            let mut arena = cell.borrow_mut();
            arena.reset(Some(component(&[("value", 1)])));
            arena.state_cells = vec![StateCell::String("old".into())];
        });
        let checkpoint = begin_reload(Some(component(&[("value", 2)]))).unwrap();
        BUILD_ARENA.with(|cell| {
            let mut arena = cell.borrow_mut();
            arena.begin_render(true);
            let value = arena.use_state(StateCell::Int(9)).unwrap();
            assert_eq!(arena.state(value), Ok(&StateCell::Int(9)));
        });
        checkpoint.commit();
        BUILD_ARENA.with(|cell| {
            assert_eq!(cell.borrow().state_cells, vec![StateCell::Int(9)]);
        });
    }

    #[test]
    fn generic_reload_preserves_state_by_declaration_order_and_runtime_type() {
        BUILD_ARENA.with(|cell| {
            let mut arena = cell.borrow_mut();
            arena.reset(None);
            arena.state_cells = vec![StateCell::String("Ada".into()), StateCell::Bool(true)];
        });
        let checkpoint = begin_reload(None).unwrap();
        BUILD_ARENA.with(|cell| {
            let mut arena = cell.borrow_mut();
            arena.begin_render(true);
            let name = arena
                .use_state(StateCell::String("Volang".to_string()))
                .unwrap();
            let enabled = arena.use_state(StateCell::Bool(false)).unwrap();
            assert_eq!(arena.state(name), Ok(&StateCell::String("Ada".into())));
            assert_eq!(arena.state(enabled), Ok(&StateCell::Bool(true)));
        });
        checkpoint.commit();
    }

    #[test]
    fn generic_reload_preserves_keyed_component_state_by_canonical_path() {
        BUILD_ARENA.with(|cell| {
            let mut arena = cell.borrow_mut();
            arena.reset(None);
            arena.begin_render(true);
            arena
                .enter_generic_component(
                    "local/components::Counter".to_string(),
                    7,
                    Some(Key::Text("alpha".to_string())),
                )
                .unwrap();
            let state = arena.use_state(StateCell::Int(0)).unwrap();
            *arena.state_mut(state).unwrap() = StateCell::Int(12);
            arena.exit_generic_component().unwrap();
            arena.finish_state_declarations().unwrap();
        });
        let checkpoint = begin_reload(None).unwrap();
        BUILD_ARENA.with(|cell| {
            let mut arena = cell.borrow_mut();
            arena.begin_render(true);
            arena
                .enter_generic_component(
                    "local/components::Counter".to_string(),
                    99,
                    Some(Key::Text("alpha".to_string())),
                )
                .unwrap();
            let state = arena.use_state(StateCell::Int(0)).unwrap();
            assert_eq!(arena.state(state), Ok(&StateCell::Int(12)));
            arena.exit_generic_component().unwrap();
            arena.finish_state_declarations().unwrap();
        });
        checkpoint.commit();
    }

    #[test]
    fn unknown_official_extern_fails_closed() {
        let externs = vec![extern_def("FutureControl")];
        let mut registry = ExternRegistry::new();
        let error = register_externs(&mut registry, &externs).unwrap_err();
        assert!(error.message().contains("FutureControl"));
    }
}
