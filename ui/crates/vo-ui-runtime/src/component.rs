use super::{
    emit_slot_mutations, emit_template_detach, prepare_template, MountedTemplate, NodeAllocator,
};
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_artifact::{
    validate_component_bundle, BundleError, BundleLimits, ComponentBundle, ComponentCallMode,
    ComponentCallSite, ComponentCallSiteId, ComponentDefinition, ComponentTypeId, EffectId,
    HandlerSiteId, StateFieldId, TaskSiteId,
};
use vo_ui_core::{HandlerId, Key, NodeId};
use vo_ui_plan::{SlotId, SlotValue};
use vo_ui_protocol::{Mutation, MutationBatch, Renderer};

type DefinitionSites = (Vec<HandlerSiteId>, Vec<EffectId>, Vec<TaskSiteId>);

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentInstanceId {
    index: u32,
    generation: u32,
}

impl ComponentInstanceId {
    pub const fn new(index: u32, generation: u32) -> Self {
        Self { index, generation }
    }

    pub const fn index(self) -> u32 {
        self.index
    }

    pub const fn generation(self) -> u32 {
        self.generation
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ComponentForestLimits {
    pub max_instances: usize,
    pub max_children_per_instance: usize,
    pub max_depth: usize,
    pub max_props_per_instance: usize,
    pub max_states_per_instance: usize,
    pub max_handlers_per_instance: usize,
    pub max_effects_per_instance: usize,
    pub max_tasks_per_instance: usize,
    pub max_slots_per_instance: usize,
}

impl Default for ComponentForestLimits {
    fn default() -> Self {
        Self {
            max_instances: 65_536,
            max_children_per_instance: 16_384,
            max_depth: 256,
            max_props_per_instance: u16::MAX as usize,
            max_states_per_instance: 65_536,
            max_handlers_per_instance: 200_000,
            max_effects_per_instance: 65_536,
            max_tasks_per_instance: 65_536,
            max_slots_per_instance: 100_000,
        }
    }
}

/// Cloneable, deterministic state representation owned by the component
/// runtime. VM/JIT/AOT adapters retain precise language values on their side
/// and may use this form for snapshots, tests, persistence, and reload.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ComponentValue {
    Unit,
    Bool(bool),
    Int(i64),
    FloatBits(u64),
    Text(String),
    Bytes(Vec<u8>),
    List(Vec<ComponentValue>),
    Record(BTreeMap<String, ComponentValue>),
}

impl ComponentValue {
    pub fn from_float(value: f64) -> Self {
        Self::FloatBits(value.to_bits())
    }

    pub const fn as_float(&self) -> Option<f64> {
        match self {
            Self::FloatBits(bits) => Some(f64::from_bits(*bits)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ComponentStateCell {
    pub field: StateFieldId,
    pub key: String,
    pub type_fingerprint: u64,
    pub value: ComponentValue,
    pub revision: u64,
}

impl ComponentStateCell {
    pub fn new(
        field: StateFieldId,
        key: impl Into<String>,
        type_fingerprint: u64,
        value: ComponentValue,
    ) -> Self {
        Self {
            field,
            key: key.into(),
            type_fingerprint,
            value,
            revision: 0,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ComponentStateWrite {
    pub field: StateFieldId,
    pub type_fingerprint: u64,
    pub value: ComponentValue,
}

impl ComponentStateWrite {
    pub const fn new(field: StateFieldId, type_fingerprint: u64, value: ComponentValue) -> Self {
        Self {
            field,
            type_fingerprint,
            value,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ComponentStateCommit {
    pub revision: u64,
    pub instance: ComponentInstanceId,
    pub changed: Vec<StateFieldId>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ComponentHandlerId {
    pub instance: ComponentInstanceId,
    pub site: HandlerSiteId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentSpec {
    pub call_site: ComponentCallSiteId,
    pub type_id: ComponentTypeId,
    pub key: Option<Key>,
    pub props: Vec<ComponentValue>,
    pub state: Vec<ComponentStateCell>,
    pub handlers: Vec<HandlerSiteId>,
    pub effects: Vec<EffectId>,
    pub tasks: Vec<TaskSiteId>,
    pub slots: Vec<SlotValue>,
    pub children: Vec<ComponentSpec>,
}

impl ComponentSpec {
    pub fn new(call_site: ComponentCallSiteId, type_id: ComponentTypeId) -> Self {
        Self {
            call_site,
            type_id,
            key: None,
            props: Vec::new(),
            state: Vec::new(),
            handlers: Vec::new(),
            effects: Vec::new(),
            tasks: Vec::new(),
            slots: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn keyed(mut self, key: impl Into<Key>) -> Self {
        self.key = Some(key.into());
        self
    }

    pub fn children(mut self, children: impl IntoIterator<Item = ComponentSpec>) -> Self {
        self.children.extend(children);
        self
    }

    pub fn props(mut self, props: impl IntoIterator<Item = ComponentValue>) -> Self {
        self.props.extend(props);
        self
    }

    pub fn state(mut self, state: impl IntoIterator<Item = ComponentStateCell>) -> Self {
        self.state.extend(state);
        self
    }

    pub fn handlers(mut self, handlers: impl IntoIterator<Item = HandlerSiteId>) -> Self {
        self.handlers.extend(handlers);
        self
    }

    pub fn effects(mut self, effects: impl IntoIterator<Item = EffectId>) -> Self {
        self.effects.extend(effects);
        self
    }

    pub fn tasks(mut self, tasks: impl IntoIterator<Item = TaskSiteId>) -> Self {
        self.tasks.extend(tasks);
        self
    }

    pub fn slots(mut self, slots: impl IntoIterator<Item = SlotValue>) -> Self {
        self.slots.extend(slots);
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MountedComponent {
    pub id: ComponentInstanceId,
    pub type_id: ComponentTypeId,
    pub parent: Option<ComponentInstanceId>,
    pub call_site: Option<ComponentCallSiteId>,
    pub key: Option<Key>,
    pub sibling_index: u32,
    pub props: Vec<ComponentValue>,
    pub state: Vec<ComponentStateCell>,
    pub handlers: Vec<HandlerSiteId>,
    pub effects: Vec<EffectId>,
    pub tasks: Vec<TaskSiteId>,
    pub slots: Vec<SlotValue>,
    pub children: Vec<ComponentInstanceId>,
    pub mounted_revision: u64,
    pub updated_revision: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ComponentReplacement {
    pub previous: ComponentInstanceId,
    pub next: ComponentInstanceId,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ComponentForestCommit {
    pub revision: u64,
    /// Preorder, so owners exist before their descendants initialize.
    pub created: Vec<ComponentInstanceId>,
    /// Preorder in the accepted candidate tree.
    pub reused: Vec<ComponentInstanceId>,
    pub moved: Vec<ComponentInstanceId>,
    pub replacements: Vec<ComponentReplacement>,
    pub props_changed: Vec<ComponentInstanceId>,
    /// Postorder, so descendants invalidate before their owner cleans up.
    pub disposed: Vec<ComponentInstanceId>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ComponentForestError {
    InvalidConfiguration,
    AlreadyMounted,
    NotMounted,
    StaleInstance(ComponentInstanceId),
    DuplicateKey(Key),
    DuplicateCallSite(ComponentCallSiteId),
    ChildLimitExceeded,
    InstanceLimitExceeded,
    IdentityExhausted,
    DepthLimitExceeded,
    PropLimitExceeded,
    StateLimitExceeded,
    HandlerLimitExceeded,
    EffectLimitExceeded,
    TaskLimitExceeded,
    SlotLimitExceeded,
    StatesNotCanonical,
    DuplicateStateKey(String),
    HandlersNotCanonical,
    EffectsNotCanonical,
    TasksNotCanonical,
    MissingState(StateFieldId),
    StateTypeMismatch(StateFieldId),
}

impl fmt::Display for ComponentForestError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "component forest rejected candidate: {self:?}")
    }
}

#[derive(Clone, Debug)]
struct InstanceSlot {
    generation: u32,
    instance: Option<MountedComponent>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum SiblingIdentity {
    Key(Key),
    CallSite(ComponentCallSiteId),
}

impl SiblingIdentity {
    fn from_spec(spec: &ComponentSpec) -> Self {
        match &spec.key {
            Some(key) => Self::Key(key.clone()),
            None => Self::CallSite(spec.call_site),
        }
    }

    fn from_instance(instance: &MountedComponent) -> Self {
        match &instance.key {
            Some(key) => Self::Key(key.clone()),
            None => Self::CallSite(
                instance
                    .call_site
                    .expect("only the component root lacks a call-site identity"),
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ComponentForest {
    limits: ComponentForestLimits,
    revision: u64,
    root: Option<ComponentInstanceId>,
    slots: Vec<InstanceSlot>,
    free: Vec<u32>,
    live: usize,
}

impl ComponentForest {
    pub fn new(limits: ComponentForestLimits) -> Result<Self, ComponentForestError> {
        if limits.max_instances == 0
            || limits.max_children_per_instance == 0
            || limits.max_depth == 0
            || limits.max_props_per_instance == 0
            || limits.max_states_per_instance == 0
            || limits.max_handlers_per_instance == 0
            || limits.max_effects_per_instance == 0
            || limits.max_tasks_per_instance == 0
            || limits.max_slots_per_instance == 0
        {
            return Err(ComponentForestError::InvalidConfiguration);
        }
        Ok(Self {
            limits,
            revision: 0,
            root: None,
            slots: Vec::new(),
            free: Vec::new(),
            live: 0,
        })
    }

    pub const fn revision(&self) -> u64 {
        self.revision
    }

    pub const fn root(&self) -> Option<ComponentInstanceId> {
        self.root
    }

    pub const fn live_count(&self) -> usize {
        self.live
    }

    pub fn get(&self, id: ComponentInstanceId) -> Option<&MountedComponent> {
        let slot = self.slots.get(id.index as usize)?;
        (slot.generation == id.generation)
            .then_some(slot.instance.as_ref())
            .flatten()
    }

    pub fn contains(&self, id: ComponentInstanceId) -> bool {
        self.get(id).is_some()
    }

    fn preorder(&self) -> Result<Vec<ComponentInstanceId>, ComponentForestError> {
        let Some(root) = self.root else {
            return Ok(Vec::new());
        };
        let mut result = Vec::with_capacity(self.live);
        let mut pending = alloc::vec![root];
        while let Some(instance) = pending.pop() {
            result.push(instance);
            for child in self.instance(instance)?.children.iter().rev() {
                pending.push(*child);
            }
        }
        Ok(result)
    }

    fn postorder(&self) -> Result<Vec<ComponentInstanceId>, ComponentForestError> {
        let mut result = self.preorder()?;
        result.reverse();
        Ok(result)
    }

    pub fn handler(
        &self,
        instance: ComponentInstanceId,
        site: HandlerSiteId,
    ) -> Option<ComponentHandlerId> {
        self.get(instance)?
            .handlers
            .binary_search(&site)
            .ok()
            .map(|_| ComponentHandlerId { instance, site })
    }

    pub fn resolve_handler(&self, handler: ComponentHandlerId) -> Option<&MountedComponent> {
        self.handler(handler.instance, handler.site)?;
        self.get(handler.instance)
    }

    pub fn mount(
        &mut self,
        root_type: ComponentTypeId,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentForestCommit, ComponentForestError> {
        self.mount_with_state(
            root_type,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            children,
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub fn mount_with_state(
        &mut self,
        root_type: ComponentTypeId,
        root_props: Vec<ComponentValue>,
        root_state: Vec<ComponentStateCell>,
        root_handlers: Vec<HandlerSiteId>,
        root_effects: Vec<EffectId>,
        root_tasks: Vec<TaskSiteId>,
        root_slots: Vec<SlotValue>,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentForestCommit, ComponentForestError> {
        if self.root.is_some() {
            return Err(ComponentForestError::AlreadyMounted);
        }
        let mut candidate = self.clone();
        candidate.validate_instance_contract(
            &root_props,
            &root_state,
            &root_handlers,
            &root_effects,
            &root_tasks,
            &root_slots,
        )?;
        candidate.validate_specs(&children, 1)?;
        let next_revision = candidate.revision.saturating_add(1);
        let mut commit = ComponentForestCommit {
            revision: next_revision,
            ..ComponentForestCommit::default()
        };
        let root = candidate.allocate(MountedComponent {
            id: ComponentInstanceId::new(0, 0),
            type_id: root_type,
            parent: None,
            call_site: None,
            key: None,
            sibling_index: 0,
            props: root_props,
            state: root_state,
            handlers: root_handlers,
            effects: root_effects,
            tasks: root_tasks,
            slots: root_slots,
            children: Vec::new(),
            mounted_revision: next_revision,
            updated_revision: next_revision,
        })?;
        candidate.root = Some(root);
        commit.created.push(root);
        candidate.mount_children(root, &children, next_revision, &mut commit)?;
        candidate.revision = next_revision;
        *self = candidate;
        Ok(commit)
    }

    /// Applies a complete state transaction to one live instance. Every field
    /// and logical type is checked before the candidate publishes.
    pub fn write_state(
        &mut self,
        instance: ComponentInstanceId,
        writes: Vec<ComponentStateWrite>,
    ) -> Result<ComponentStateCommit, ComponentForestError> {
        self.instance(instance)?;
        let mut seen = BTreeSet::new();
        for write in &writes {
            if !seen.insert(write.field) {
                return Err(ComponentForestError::StatesNotCanonical);
            }
        }
        let mut candidate = self.clone();
        let revision = candidate.revision.saturating_add(1);
        let mut changed = Vec::new();
        {
            let mounted = candidate.instance_mut(instance)?;
            for write in writes {
                let cell = mounted
                    .state
                    .get_mut(write.field.value() as usize)
                    .filter(|cell| cell.field == write.field)
                    .ok_or(ComponentForestError::MissingState(write.field))?;
                if cell.type_fingerprint != write.type_fingerprint {
                    return Err(ComponentForestError::StateTypeMismatch(write.field));
                }
                if cell.value != write.value {
                    cell.value = write.value;
                    cell.revision = revision;
                    changed.push(write.field);
                }
            }
            mounted.updated_revision = revision;
        }
        candidate.revision = revision;
        *self = candidate;
        Ok(ComponentStateCommit {
            revision,
            instance,
            changed,
        })
    }

    pub fn reconcile(
        &mut self,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentForestCommit, ComponentForestError> {
        let root = self.root.ok_or(ComponentForestError::NotMounted)?;
        self.reconcile_children(root, children)
    }

    pub fn reconcile_children(
        &mut self,
        parent: ComponentInstanceId,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentForestCommit, ComponentForestError> {
        self.instance(parent)?;
        let mut candidate = self.clone();
        candidate.validate_specs(&children, 1)?;
        let next_revision = candidate.revision.saturating_add(1);
        let mut commit = ComponentForestCommit {
            revision: next_revision,
            ..ComponentForestCommit::default()
        };
        candidate.stage_children(parent, &children, next_revision, &mut commit)?;
        if let Some(root) = candidate.root {
            candidate.instance_mut(root)?.updated_revision = next_revision;
        }
        candidate.revision = next_revision;
        *self = candidate;
        Ok(commit)
    }

    pub fn unmount(&mut self) -> Result<ComponentForestCommit, ComponentForestError> {
        let root = self.root.ok_or(ComponentForestError::NotMounted)?;
        let mut candidate = self.clone();
        let next_revision = candidate.revision.saturating_add(1);
        let mut commit = ComponentForestCommit {
            revision: next_revision,
            ..ComponentForestCommit::default()
        };
        candidate.dispose_subtree(root, &mut commit.disposed)?;
        candidate.root = None;
        candidate.revision = next_revision;
        *self = candidate;
        Ok(commit)
    }

    fn validate_specs(
        &self,
        specs: &[ComponentSpec],
        depth: usize,
    ) -> Result<(), ComponentForestError> {
        if depth > self.limits.max_depth {
            return Err(ComponentForestError::DepthLimitExceeded);
        }
        if specs.len() > self.limits.max_children_per_instance {
            return Err(ComponentForestError::ChildLimitExceeded);
        }
        let mut identities = BTreeSet::new();
        for spec in specs {
            self.validate_instance_contract(
                &spec.props,
                &spec.state,
                &spec.handlers,
                &spec.effects,
                &spec.tasks,
                &spec.slots,
            )?;
            let identity = SiblingIdentity::from_spec(spec);
            if !identities.insert(identity.clone()) {
                return Err(match identity {
                    SiblingIdentity::Key(key) => ComponentForestError::DuplicateKey(key),
                    SiblingIdentity::CallSite(call_site) => {
                        ComponentForestError::DuplicateCallSite(call_site)
                    }
                });
            }
            self.validate_specs(&spec.children, depth.saturating_add(1))?;
        }
        Ok(())
    }

    fn validate_instance_contract(
        &self,
        props: &[ComponentValue],
        state: &[ComponentStateCell],
        handlers: &[HandlerSiteId],
        effects: &[EffectId],
        tasks: &[TaskSiteId],
        slots: &[SlotValue],
    ) -> Result<(), ComponentForestError> {
        if props.len() > self.limits.max_props_per_instance {
            return Err(ComponentForestError::PropLimitExceeded);
        }
        if state.len() > self.limits.max_states_per_instance {
            return Err(ComponentForestError::StateLimitExceeded);
        }
        if handlers.len() > self.limits.max_handlers_per_instance {
            return Err(ComponentForestError::HandlerLimitExceeded);
        }
        if effects.len() > self.limits.max_effects_per_instance {
            return Err(ComponentForestError::EffectLimitExceeded);
        }
        if tasks.len() > self.limits.max_tasks_per_instance {
            return Err(ComponentForestError::TaskLimitExceeded);
        }
        if slots.len() > self.limits.max_slots_per_instance {
            return Err(ComponentForestError::SlotLimitExceeded);
        }
        let mut keys = BTreeSet::new();
        for (index, cell) in state.iter().enumerate() {
            if cell.field.value() as usize != index {
                return Err(ComponentForestError::StatesNotCanonical);
            }
            if !keys.insert(cell.key.as_str()) {
                return Err(ComponentForestError::DuplicateStateKey(cell.key.clone()));
            }
        }
        if handlers.windows(2).any(|pair| pair[0] >= pair[1]) {
            return Err(ComponentForestError::HandlersNotCanonical);
        }
        if effects.windows(2).any(|pair| pair[0] >= pair[1]) {
            return Err(ComponentForestError::EffectsNotCanonical);
        }
        if tasks.windows(2).any(|pair| pair[0] >= pair[1]) {
            return Err(ComponentForestError::TasksNotCanonical);
        }
        Ok(())
    }

    fn stage_children(
        &mut self,
        parent: ComponentInstanceId,
        specs: &[ComponentSpec],
        revision: u64,
        commit: &mut ComponentForestCommit,
    ) -> Result<(), ComponentForestError> {
        let previous = self.instance(parent)?.children.clone();
        let mut by_identity = BTreeMap::new();
        for child in &previous {
            by_identity.insert(
                SiblingIdentity::from_instance(self.instance(*child)?),
                *child,
            );
        }
        let mut accepted = Vec::new();
        accepted
            .try_reserve_exact(specs.len())
            .map_err(|_| ComponentForestError::InstanceLimitExceeded)?;
        for (position, spec) in specs.iter().enumerate() {
            let position =
                u32::try_from(position).map_err(|_| ComponentForestError::IdentityExhausted)?;
            let identity = SiblingIdentity::from_spec(spec);
            if let Some(previous_id) = by_identity.remove(&identity) {
                let compatible = self.instance(previous_id)?.type_id == spec.type_id;
                if compatible {
                    let old_position = self.instance(previous_id)?.sibling_index;
                    let props_changed = self.instance(previous_id)?.props != spec.props;
                    {
                        let instance = self.instance_mut(previous_id)?;
                        instance.sibling_index = position;
                        instance.props.clone_from(&spec.props);
                        instance.handlers.clone_from(&spec.handlers);
                        instance.effects.clone_from(&spec.effects);
                        instance.tasks.clone_from(&spec.tasks);
                        instance.slots.clone_from(&spec.slots);
                        instance.updated_revision = revision;
                    }
                    commit.reused.push(previous_id);
                    if props_changed {
                        commit.props_changed.push(previous_id);
                    }
                    if old_position != position {
                        commit.moved.push(previous_id);
                    }
                    self.stage_children(previous_id, &spec.children, revision, commit)?;
                    accepted.push(previous_id);
                    continue;
                }
                let next = self.mount_spec(parent, position, spec, revision, commit)?;
                commit.replacements.push(ComponentReplacement {
                    previous: previous_id,
                    next,
                });
                self.dispose_subtree(previous_id, &mut commit.disposed)?;
                accepted.push(next);
                continue;
            }
            accepted.push(self.mount_spec(parent, position, spec, revision, commit)?);
        }
        for stale in by_identity.into_values() {
            self.dispose_subtree(stale, &mut commit.disposed)?;
        }
        self.instance_mut(parent)?.children = accepted;
        self.instance_mut(parent)?.updated_revision = revision;
        Ok(())
    }

    fn mount_children(
        &mut self,
        parent: ComponentInstanceId,
        specs: &[ComponentSpec],
        revision: u64,
        commit: &mut ComponentForestCommit,
    ) -> Result<(), ComponentForestError> {
        let mut children = Vec::new();
        children
            .try_reserve_exact(specs.len())
            .map_err(|_| ComponentForestError::InstanceLimitExceeded)?;
        for (position, spec) in specs.iter().enumerate() {
            children.push(self.mount_spec(
                parent,
                u32::try_from(position).map_err(|_| ComponentForestError::IdentityExhausted)?,
                spec,
                revision,
                commit,
            )?);
        }
        self.instance_mut(parent)?.children = children;
        Ok(())
    }

    fn mount_spec(
        &mut self,
        parent: ComponentInstanceId,
        position: u32,
        spec: &ComponentSpec,
        revision: u64,
        commit: &mut ComponentForestCommit,
    ) -> Result<ComponentInstanceId, ComponentForestError> {
        let id = self.allocate(MountedComponent {
            id: ComponentInstanceId::new(0, 0),
            type_id: spec.type_id.clone(),
            parent: Some(parent),
            call_site: Some(spec.call_site),
            key: spec.key.clone(),
            sibling_index: position,
            props: spec.props.clone(),
            state: spec.state.clone(),
            handlers: spec.handlers.clone(),
            effects: spec.effects.clone(),
            tasks: spec.tasks.clone(),
            slots: spec.slots.clone(),
            children: Vec::new(),
            mounted_revision: revision,
            updated_revision: revision,
        })?;
        commit.created.push(id);
        self.mount_children(id, &spec.children, revision, commit)?;
        Ok(id)
    }

    fn allocate(
        &mut self,
        mut instance: MountedComponent,
    ) -> Result<ComponentInstanceId, ComponentForestError> {
        if self.live >= self.limits.max_instances {
            return Err(ComponentForestError::InstanceLimitExceeded);
        }
        let id = if let Some(index) = self.free.pop() {
            let slot = &mut self.slots[index as usize];
            ComponentInstanceId::new(index, slot.generation)
        } else {
            let index = u32::try_from(self.slots.len())
                .map_err(|_| ComponentForestError::IdentityExhausted)?;
            self.slots.push(InstanceSlot {
                generation: 1,
                instance: None,
            });
            ComponentInstanceId::new(index, 1)
        };
        instance.id = id;
        self.slots[id.index as usize].instance = Some(instance);
        self.live += 1;
        Ok(id)
    }

    fn dispose_subtree(
        &mut self,
        id: ComponentInstanceId,
        disposed: &mut Vec<ComponentInstanceId>,
    ) -> Result<(), ComponentForestError> {
        let children = self.instance(id)?.children.clone();
        for child in children {
            self.dispose_subtree(child, disposed)?;
        }
        let slot = self
            .slots
            .get_mut(id.index as usize)
            .ok_or(ComponentForestError::StaleInstance(id))?;
        if slot.generation != id.generation || slot.instance.take().is_none() {
            return Err(ComponentForestError::StaleInstance(id));
        }
        slot.generation = slot.generation.wrapping_add(1).max(1);
        self.free.push(id.index);
        self.live -= 1;
        disposed.push(id);
        Ok(())
    }

    fn instance(&self, id: ComponentInstanceId) -> Result<&MountedComponent, ComponentForestError> {
        self.get(id).ok_or(ComponentForestError::StaleInstance(id))
    }

    fn instance_mut(
        &mut self,
        id: ComponentInstanceId,
    ) -> Result<&mut MountedComponent, ComponentForestError> {
        let slot = self
            .slots
            .get_mut(id.index as usize)
            .ok_or(ComponentForestError::StaleInstance(id))?;
        if slot.generation != id.generation {
            return Err(ComponentForestError::StaleInstance(id));
        }
        slot.instance
            .as_mut()
            .ok_or(ComponentForestError::StaleInstance(id))
    }
}

impl Default for ComponentForest {
    fn default() -> Self {
        Self::new(ComponentForestLimits::default())
            .expect("default component forest limits are valid")
    }
}

#[derive(Debug)]
pub enum ComponentRenderError<E> {
    Bundle(BundleError),
    Forest(ComponentForestError),
    Renderer(E),
    RootTypeMismatch(ComponentTypeId),
    MissingDefinition(ComponentTypeId),
    MissingCallSite(ComponentCallSiteId),
    IncompatibleCallSite(ComponentCallSiteId),
    InvalidPropArity(ComponentTypeId),
    InvalidStateSchema(ComponentTypeId),
    InvalidSlotValues(ComponentTypeId),
    InvalidDeclaredSites(ComponentTypeId),
}

impl<E: fmt::Debug + fmt::Display> fmt::Display for ComponentRenderError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "component renderer rejected revision: ")?;
        match self {
            Self::Renderer(error) => fmt::Display::fmt(error, formatter),
            other => write!(formatter, "{other:?}"),
        }
    }
}

impl<E> From<ComponentForestError> for ComponentRenderError<E> {
    fn from(error: ComponentForestError) -> Self {
        Self::Forest(error)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentRenderCommit {
    pub forest: ComponentForestCommit,
    pub batch: MutationBatch,
}

/// Executes one authenticated component bundle as a persistent template
/// forest. Component identity changes and renderer mutations share one staged
/// candidate, so a renderer rejection retains both previous forests.
pub struct ComponentTemplateRuntime<R: Renderer> {
    renderer: R,
    root_node: NodeId,
    session_epoch: u64,
    revision: u64,
    bundle: ComponentBundle,
    forest: ComponentForest,
    allocator: NodeAllocator,
    templates: BTreeMap<ComponentInstanceId, MountedTemplate>,
    node_owners: BTreeMap<NodeId, ComponentInstanceId>,
    last_batch: Option<MutationBatch>,
}

impl<R: Renderer> ComponentTemplateRuntime<R> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        renderer: R,
        session_epoch: u64,
        root_node: NodeId,
        bundle: ComponentBundle,
        bundle_limits: BundleLimits,
        forest_limits: ComponentForestLimits,
    ) -> Result<Self, ComponentRenderError<R::Error>> {
        validate_component_bundle(&bundle, bundle_limits).map_err(ComponentRenderError::Bundle)?;
        Ok(Self {
            renderer,
            root_node,
            session_epoch,
            revision: 0,
            bundle,
            forest: ComponentForest::new(forest_limits)?,
            allocator: NodeAllocator::with_reserved(root_node),
            templates: BTreeMap::new(),
            node_owners: BTreeMap::new(),
            last_batch: None,
        })
    }

    pub const fn revision(&self) -> u64 {
        self.revision
    }

    pub fn renderer(&self) -> &R {
        &self.renderer
    }

    pub fn renderer_mut(&mut self) -> &mut R {
        &mut self.renderer
    }

    pub fn forest(&self) -> &ComponentForest {
        &self.forest
    }

    /// Resolves a renderer listener back to the logical component instance
    /// and source handler site that own it. Node identities are unique across
    /// the mounted template forest, so this mapping remains stable when a
    /// sibling component moves or updates in place.
    pub fn resolve_event_handler(
        &self,
        target: NodeId,
        plan_handler: HandlerId,
    ) -> Option<ComponentHandlerId> {
        let instance = *self.node_owners.get(&target)?;
        let mounted = self.forest.get(instance)?;
        let definition = self.definition(&mounted.type_id).ok()?;
        let site = definition
            .handlers
            .iter()
            .find(|handler| handler.plan_handler == plan_handler)?
            .id;
        self.forest.handler(instance, site)
    }

    /// Resolves a static source call path to one live logical handler. This is
    /// used by language adapters while they lease freshly evaluated closures.
    pub fn handler_at_path(
        &self,
        path: &[ComponentCallSiteId],
        site: HandlerSiteId,
    ) -> Option<ComponentHandlerId> {
        let mut instance = self.forest.root()?;
        for call_site in path {
            instance = self
                .forest
                .get(instance)?
                .children
                .iter()
                .copied()
                .find(|child| {
                    self.forest
                        .get(*child)
                        .is_some_and(|mounted| mounted.call_site == Some(*call_site))
                })?;
        }
        self.forest.handler(instance, site)
    }

    pub fn write_state(
        &mut self,
        instance: ComponentInstanceId,
        writes: Vec<ComponentStateWrite>,
    ) -> Result<ComponentStateCommit, ComponentRenderError<R::Error>> {
        self.forest
            .write_state(instance, writes)
            .map_err(Into::into)
    }

    pub fn last_batch(&self) -> Option<&MutationBatch> {
        self.last_batch.as_ref()
    }

    pub fn into_renderer(self) -> R {
        self.renderer
    }

    pub fn mount(
        &mut self,
        root_props: Vec<ComponentValue>,
        root_state: Vec<ComponentStateCell>,
        root_slots: Vec<SlotValue>,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        let root_type = self.bundle.root.clone();
        let (root_handlers, root_effects, root_tasks) = self.definition_sites_owned(&root_type)?;
        self.validate_instance_values(&root_type, &root_props, &root_state, &root_slots)?;
        let children = self.normalize_children(&root_type, children)?;
        let mut candidate = self.forest.clone();
        let commit = candidate.mount_with_state(
            root_type,
            root_props,
            root_state,
            root_handlers,
            root_effects,
            root_tasks,
            root_slots,
            children,
        )?;
        self.apply_candidate(candidate, commit)
    }

    /// Re-evaluates the current bundle while retaining compatible component
    /// instances, state, and template node identities. The supplied state is
    /// used only for newly created instances; live cells remain owned by the
    /// forest until an explicit state transaction changes them.
    pub fn update(
        &mut self,
        root_props: Vec<ComponentValue>,
        root_state: Vec<ComponentStateCell>,
        root_slots: Vec<SlotValue>,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        let root = self.forest.root().ok_or(ComponentForestError::NotMounted)?;
        let root_type = self.bundle.root.clone();
        self.validate_instance_values(&root_type, &root_props, &root_state, &root_slots)?;
        let children = self.normalize_children(&root_type, children)?;
        let mut candidate = self.forest.clone();
        let mut commit = candidate.reconcile(children)?;
        let props_changed = candidate
            .get(root)
            .is_some_and(|mounted| mounted.props != root_props);
        {
            let mounted = candidate.instance_mut(root)?;
            mounted.props = root_props;
            mounted.slots = root_slots;
        }
        commit.reused.insert(0, root);
        if props_changed {
            commit.props_changed.push(root);
        }
        self.apply_candidate(candidate, commit)
    }

    /// Applies coalesced direct slot writes to the mounted root component
    /// while retaining the component forest, handler identities, and every
    /// template node. Renderer acceptance precedes publication of the new
    /// slots, so a failed batch leaves the prior forest fully observable.
    pub fn update_root_slots(
        &mut self,
        updates: impl IntoIterator<Item = (SlotId, SlotValue)>,
    ) -> Result<Option<MutationBatch>, ComponentRenderError<R::Error>> {
        let root = self.forest.root().ok_or(ComponentForestError::NotMounted)?;
        let mounted = self
            .forest
            .get(root)
            .expect("mounted component forest root is live");
        let root_type = mounted.type_id.clone();
        let mut slots = mounted.slots.clone();
        for (slot, value) in updates {
            let Some(target) = slots.get_mut(slot.index() as usize) else {
                return Err(ComponentRenderError::InvalidSlotValues(root_type));
            };
            *target = value;
        }
        self.validate_instance_values(&root_type, &mounted.props, &mounted.state, &slots)?;

        let template = self
            .templates
            .get(&root)
            .expect("mounted component forest root retains its template");
        let mut mutations = Vec::new();
        for (index, value) in slots.iter().enumerate() {
            if template.slots[index] != *value {
                emit_slot_mutations(
                    &template.plan,
                    &template.nodes,
                    SlotId::new(index as u32),
                    value,
                    &mut mutations,
                );
            }
        }
        if mutations.is_empty() {
            return Ok(None);
        }

        let batch = MutationBatch::new(
            self.session_epoch,
            self.revision.saturating_add(1),
            mutations,
        );
        self.renderer
            .apply(&batch)
            .map_err(ComponentRenderError::Renderer)?;
        self.forest.instance_mut(root)?.slots = slots.clone();
        self.templates
            .get_mut(&root)
            .expect("mounted component forest root retains its template")
            .slots = slots;
        self.revision = batch.revision;
        self.last_batch = Some(batch.clone());
        Ok(Some(batch))
    }

    pub fn reconcile(
        &mut self,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        let root = self.forest.root().ok_or(ComponentForestError::NotMounted)?;
        let root_type = self
            .forest
            .get(root)
            .expect("mounted forest root is live")
            .type_id
            .clone();
        let children = self.normalize_children(&root_type, children)?;
        let mut candidate = self.forest.clone();
        let commit = candidate.reconcile(children)?;
        self.apply_candidate(candidate, commit)
    }

    pub fn unmount(&mut self) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        let mut candidate = self.forest.clone();
        let commit = candidate.unmount()?;
        self.apply_candidate(candidate, commit)
    }

    /// Stages a development bundle against the current instance forest.
    /// Compatible state fields retain values by source key and logical type;
    /// the visible template forest swaps in one renderer revision.
    pub fn reload_bundle(
        &mut self,
        bundle: ComponentBundle,
        bundle_limits: BundleLimits,
        root_props: Vec<ComponentValue>,
        root_state: Vec<ComponentStateCell>,
        root_slots: Vec<SlotValue>,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        validate_component_bundle(&bundle, bundle_limits).map_err(ComponentRenderError::Bundle)?;
        if bundle.root != self.bundle.root {
            return Err(ComponentRenderError::RootTypeMismatch(bundle.root));
        }
        let previous_bundle = core::mem::replace(&mut self.bundle, bundle);
        let result = self.prepare_reload(
            &previous_bundle,
            root_props,
            root_state,
            root_slots,
            children,
        );
        if result.is_err() {
            self.bundle = previous_bundle;
        }
        result
    }

    fn prepare_reload(
        &mut self,
        previous_bundle: &ComponentBundle,
        root_props: Vec<ComponentValue>,
        root_state: Vec<ComponentStateCell>,
        root_slots: Vec<SlotValue>,
        children: Vec<ComponentSpec>,
    ) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        let root = self.forest.root().ok_or(ComponentForestError::NotMounted)?;
        let root_type = self.bundle.root.clone();
        self.validate_instance_values(&root_type, &root_props, &root_state, &root_slots)?;
        let children = self.normalize_children(&root_type, children)?;
        let mut candidate = self.forest.clone();
        let commit = candidate.reconcile(children.clone())?;
        let (handlers, effects, tasks) = self.definition_sites_owned(&root_type)?;
        {
            let mounted = candidate.instance_mut(root)?;
            mounted.props = root_props;
            mounted.handlers = handlers;
            mounted.effects = effects;
            mounted.tasks = tasks;
            mounted.slots = root_slots;
        }

        let mut desired_state = BTreeMap::new();
        desired_state.insert(root, root_state);
        Self::collect_desired_state(&candidate, root, &children, &mut desired_state)?;
        for instance in candidate.preorder()? {
            let Some(initial) = desired_state.remove(&instance) else {
                continue;
            };
            let previous = self
                .forest
                .get(instance)
                .map(|mounted| mounted.state.as_slice())
                .unwrap_or_default();
            candidate.instance_mut(instance)?.state = migrate_state(previous, initial);
        }
        self.apply_full_reload_candidate(previous_bundle, candidate, commit)
    }

    fn collect_desired_state(
        forest: &ComponentForest,
        parent: ComponentInstanceId,
        specs: &[ComponentSpec],
        output: &mut BTreeMap<ComponentInstanceId, Vec<ComponentStateCell>>,
    ) -> Result<(), ComponentForestError> {
        let children = &forest.instance(parent)?.children;
        debug_assert_eq!(children.len(), specs.len());
        for (instance, spec) in children.iter().zip(specs) {
            output.insert(*instance, spec.state.clone());
            Self::collect_desired_state(forest, *instance, &spec.children, output)?;
        }
        Ok(())
    }

    fn normalize_children(
        &self,
        parent_type: &ComponentTypeId,
        children: Vec<ComponentSpec>,
    ) -> Result<Vec<ComponentSpec>, ComponentRenderError<R::Error>> {
        let parent = self.definition(parent_type)?;
        let mut normalized = Vec::new();
        for mut spec in children {
            let call = parent
                .call_sites
                .binary_search_by_key(&spec.call_site, |call| call.id)
                .ok()
                .map(|index| &parent.call_sites[index])
                .ok_or(ComponentRenderError::MissingCallSite(spec.call_site))?;
            match call.mode {
                ComponentCallMode::Static if call.callee.as_ref() != Some(&spec.type_id) => {
                    return Err(ComponentRenderError::IncompatibleCallSite(spec.call_site));
                }
                ComponentCallMode::Dynamic if call.callee.is_some() => {
                    return Err(ComponentRenderError::IncompatibleCallSite(spec.call_site));
                }
                ComponentCallMode::Static | ComponentCallMode::Dynamic => {}
            }
            self.validate_instance_values(&spec.type_id, &spec.props, &spec.state, &spec.slots)?;
            let (handlers, effects, tasks) = self.definition_sites_owned(&spec.type_id)?;
            spec.handlers = handlers;
            spec.effects = effects;
            spec.tasks = tasks;
            spec.children = self.normalize_children(&spec.type_id, spec.children)?;
            normalized.push(spec);
        }
        Ok(normalized)
    }

    fn validate_instance_values(
        &self,
        type_id: &ComponentTypeId,
        props: &[ComponentValue],
        state: &[ComponentStateCell],
        slots: &[SlotValue],
    ) -> Result<(), ComponentRenderError<R::Error>> {
        let definition = self.definition(type_id)?;
        if props.len() != usize::from(definition.interface.props_arity) {
            return Err(ComponentRenderError::InvalidPropArity(type_id.clone()));
        }
        if state.len() != definition.states.len()
            || state.iter().zip(&definition.states).any(|(cell, field)| {
                cell.field != field.id
                    || cell.key != field.key
                    || cell.type_fingerprint != field.type_fingerprint
            })
        {
            return Err(ComponentRenderError::InvalidStateSchema(type_id.clone()));
        }
        if slots.len() != definition.plan.slots().len()
            || slots
                .iter()
                .zip(definition.plan.slots())
                .any(|(value, kind)| value.kind() != *kind)
        {
            return Err(ComponentRenderError::InvalidSlotValues(type_id.clone()));
        }
        Ok(())
    }

    fn definition(
        &self,
        type_id: &ComponentTypeId,
    ) -> Result<&ComponentDefinition, ComponentRenderError<R::Error>> {
        self.bundle
            .definitions
            .binary_search_by(|definition| definition.type_id.cmp(type_id))
            .ok()
            .map(|index| &self.bundle.definitions[index])
            .ok_or_else(|| ComponentRenderError::MissingDefinition(type_id.clone()))
    }

    fn definition_sites_owned(
        &self,
        type_id: &ComponentTypeId,
    ) -> Result<DefinitionSites, ComponentRenderError<R::Error>> {
        let definition = self.definition(type_id)?;
        Ok((
            definition
                .handlers
                .iter()
                .map(|handler| handler.id)
                .collect(),
            definition.effects.iter().map(|effect| effect.id).collect(),
            definition.tasks.iter().map(|task| task.id).collect(),
        ))
    }

    fn apply_full_reload_candidate(
        &mut self,
        previous_bundle: &ComponentBundle,
        candidate: ComponentForest,
        commit: ComponentForestCommit,
    ) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        let mut allocator = self.allocator.clone();
        let mut templates = BTreeMap::new();
        let mut node_owners = BTreeMap::new();
        let mut mutations = Vec::new();
        let preorder = candidate.preorder()?;
        for instance in &preorder {
            let mounted = candidate
                .get(*instance)
                .expect("reload candidate component is live");
            let definition = self.definition(&mounted.type_id)?;
            let (nodes, created) =
                prepare_template::<R::Error>(&definition.plan, &mounted.slots, &mut allocator)
                    .map_err(|_| {
                        ComponentRenderError::InvalidSlotValues(mounted.type_id.clone())
                    })?;
            mutations.extend(created);
            for node in &nodes {
                node_owners.insert(*node, *instance);
            }
            templates.insert(
                *instance,
                MountedTemplate {
                    plan: definition.plan.clone(),
                    nodes,
                    slots: mounted.slots.clone(),
                },
            );
        }
        let unattached = BTreeMap::new();
        for instance in &preorder {
            let (parent, before) =
                self.mount_location(&candidate, &templates, &unattached, *instance, None)?;
            let template = templates
                .get(instance)
                .expect("reload template was prepared");
            mutations.push(Mutation::InsertBefore {
                parent,
                child: template.nodes[template.plan.root().index() as usize],
                before,
            });
        }

        for instance in self.forest.postorder()? {
            let mounted = self
                .forest
                .get(instance)
                .expect("previous reload component is live");
            let template = self
                .templates
                .get(&instance)
                .expect("previous reload template is mounted");
            let parent = match mounted.parent {
                None => self.root_node,
                Some(parent) => {
                    let parent_mounted = self
                        .forest
                        .get(parent)
                        .expect("previous reload parent is live");
                    let call = call_site_in(
                        previous_bundle,
                        &parent_mounted.type_id,
                        mounted.call_site.unwrap(),
                    )?;
                    let parent_template = self
                        .templates
                        .get(&parent)
                        .expect("previous reload parent template is mounted");
                    parent_template.nodes[call.mount_parent.index() as usize]
                }
            };
            emit_template_detach(parent, template, &mut mutations);
        }

        let batch = MutationBatch::new(
            self.session_epoch,
            self.revision.saturating_add(1),
            mutations,
        );
        self.renderer
            .apply(&batch)
            .map_err(ComponentRenderError::Renderer)?;
        for template in self.templates.values() {
            for node in template.nodes.iter().rev().copied() {
                allocator.release(node);
            }
        }
        self.forest = candidate;
        self.allocator = allocator;
        self.templates = templates;
        self.node_owners = node_owners;
        self.revision = batch.revision;
        self.last_batch = Some(batch.clone());
        Ok(ComponentRenderCommit {
            forest: commit,
            batch,
        })
    }

    fn apply_candidate(
        &mut self,
        candidate: ComponentForest,
        commit: ComponentForestCommit,
    ) -> Result<ComponentRenderCommit, ComponentRenderError<R::Error>> {
        let mut allocator = self.allocator.clone();
        let mut templates = self.templates.clone();
        let mut node_owners = self.node_owners.clone();
        let mut mutations = Vec::new();

        for instance in &commit.created {
            let mounted = candidate
                .get(*instance)
                .expect("created candidate component is live");
            let definition = self.definition(&mounted.type_id)?;
            let (nodes, created) =
                prepare_template::<R::Error>(&definition.plan, &mounted.slots, &mut allocator)
                    .map_err(|_| {
                        ComponentRenderError::InvalidSlotValues(mounted.type_id.clone())
                    })?;
            mutations.extend(created);
            for node in &nodes {
                node_owners.insert(*node, *instance);
            }
            templates.insert(
                *instance,
                MountedTemplate {
                    plan: definition.plan.clone(),
                    nodes,
                    slots: mounted.slots.clone(),
                },
            );
        }

        for instance in &commit.reused {
            let mounted = candidate
                .get(*instance)
                .expect("reused candidate component is live");
            let template = templates
                .get_mut(instance)
                .expect("reused component retains its mounted template");
            for (index, value) in mounted.slots.iter().enumerate() {
                if template.slots[index] != *value {
                    emit_slot_mutations(
                        &template.plan,
                        &template.nodes,
                        SlotId::new(index as u32),
                        value,
                        &mut mutations,
                    );
                    template.slots[index] = value.clone();
                }
            }
        }

        let replacements = commit
            .replacements
            .iter()
            .map(|replacement| (replacement.next, replacement.previous))
            .collect::<BTreeMap<_, _>>();
        for instance in commit.created.iter().chain(&commit.moved) {
            let (parent, before) = self.mount_location(
                &candidate,
                &templates,
                &self.templates,
                *instance,
                replacements.get(instance).copied(),
            )?;
            let template = templates
                .get(instance)
                .expect("accepted component has prepared template");
            mutations.push(Mutation::InsertBefore {
                parent,
                child: template.nodes[template.plan.root().index() as usize],
                before,
            });
        }

        for instance in &commit.disposed {
            let mounted = self
                .forest
                .get(*instance)
                .expect("disposed component belonged to previous forest");
            let template = templates
                .get(instance)
                .expect("disposed component retains template until commit");
            let parent = match mounted.parent {
                None => self.root_node,
                Some(parent) => {
                    let parent_mounted = self
                        .forest
                        .get(parent)
                        .expect("disposed parent was live in previous forest");
                    let call =
                        self.call_site(&parent_mounted.type_id, mounted.call_site.unwrap())?;
                    let parent_template = templates
                        .get(&parent)
                        .expect("disposed parent retains template until commit");
                    parent_template.nodes[call.mount_parent.index() as usize]
                }
            };
            emit_template_detach(parent, template, &mut mutations);
        }

        let batch = MutationBatch::new(
            self.session_epoch,
            self.revision.saturating_add(1),
            mutations,
        );
        self.renderer
            .apply(&batch)
            .map_err(ComponentRenderError::Renderer)?;

        for instance in &commit.disposed {
            if let Some(template) = templates.remove(instance) {
                for node in template.nodes.iter().rev().copied() {
                    node_owners.remove(&node);
                    allocator.release(node);
                }
            }
        }
        self.forest = candidate;
        self.allocator = allocator;
        self.templates = templates;
        self.node_owners = node_owners;
        self.revision = batch.revision;
        self.last_batch = Some(batch.clone());
        Ok(ComponentRenderCommit {
            forest: commit,
            batch,
        })
    }

    fn mount_location(
        &self,
        forest: &ComponentForest,
        templates: &BTreeMap<ComponentInstanceId, MountedTemplate>,
        previously_attached: &BTreeMap<ComponentInstanceId, MountedTemplate>,
        instance: ComponentInstanceId,
        replacement: Option<ComponentInstanceId>,
    ) -> Result<(NodeId, Option<NodeId>), ComponentRenderError<R::Error>> {
        let mounted = forest
            .get(instance)
            .expect("mount location belongs to candidate forest");
        let Some(parent) = mounted.parent else {
            return Ok((self.root_node, None));
        };
        let parent_mounted = forest.get(parent).expect("candidate child has live parent");
        let call = self.call_site(&parent_mounted.type_id, mounted.call_site.unwrap())?;
        let parent_template = templates
            .get(&parent)
            .expect("candidate parent template is prepared");
        let parent_node = parent_template.nodes[call.mount_parent.index() as usize];
        if let Some(previous) = replacement {
            let template = templates
                .get(&previous)
                .expect("replacement target retains previous template");
            return Ok((
                parent_node,
                Some(template.nodes[template.plan.root().index() as usize]),
            ));
        }
        for sibling in parent_mounted
            .children
            .iter()
            .skip(mounted.sibling_index as usize + 1)
        {
            if !previously_attached.contains_key(sibling) {
                continue;
            }
            let sibling_mounted = forest.get(*sibling).expect("candidate sibling is live");
            let sibling_call =
                self.call_site(&parent_mounted.type_id, sibling_mounted.call_site.unwrap())?;
            if sibling_call.mount_parent == call.mount_parent {
                let template = templates
                    .get(sibling)
                    .expect("candidate sibling template is prepared");
                return Ok((
                    parent_node,
                    Some(template.nodes[template.plan.root().index() as usize]),
                ));
            }
        }
        let before = call
            .mount_before
            .map(|node| parent_template.nodes[node.index() as usize]);
        Ok((parent_node, before))
    }

    fn call_site(
        &self,
        parent: &ComponentTypeId,
        id: ComponentCallSiteId,
    ) -> Result<&ComponentCallSite, ComponentRenderError<R::Error>> {
        let definition = self.definition(parent)?;
        definition
            .call_sites
            .binary_search_by_key(&id, |call| call.id)
            .ok()
            .map(|index| &definition.call_sites[index])
            .ok_or(ComponentRenderError::MissingCallSite(id))
    }
}

fn migrate_state(
    previous: &[ComponentStateCell],
    mut initial: Vec<ComponentStateCell>,
) -> Vec<ComponentStateCell> {
    for next in &mut initial {
        if let Some(old) = previous
            .iter()
            .find(|old| old.key == next.key && old.type_fingerprint == next.type_fingerprint)
        {
            next.value.clone_from(&old.value);
            next.revision = old.revision;
        }
    }
    initial
}

fn call_site_in<'a, E>(
    bundle: &'a ComponentBundle,
    parent: &ComponentTypeId,
    id: ComponentCallSiteId,
) -> Result<&'a ComponentCallSite, ComponentRenderError<E>> {
    let definition = bundle
        .definitions
        .binary_search_by(|definition| definition.type_id.cmp(parent))
        .ok()
        .map(|index| &bundle.definitions[index])
        .ok_or_else(|| ComponentRenderError::MissingDefinition(parent.clone()))?;
    definition
        .call_sites
        .binary_search_by_key(&id, |call| call.id)
        .ok()
        .map(|index| &definition.call_sites[index])
        .ok_or(ComponentRenderError::MissingCallSite(id))
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString;
    use vo_ui_artifact::{
        BindingDefinition, BindingId, BundleSourceMetadata, ComponentCallSite, ComponentInterface,
        ExecutionMode, LifecycleDefinition, StateFieldDefinition, COMPONENT_BUNDLE_ABI_VERSION,
    };
    use vo_ui_core::Primitive;
    use vo_ui_plan::{ComponentPlan, LocalNodeId, PlanLimits, SlotKind, TemplateNode, UpdateSite};
    use vo_ui_protocol::{ApplyError, NodeSnapshot, ProtocolLimits, TreeMirror};

    fn typ(name: &str) -> ComponentTypeId {
        ComponentTypeId::new("github.com/acme/app", name)
    }

    fn child(site: u64, name: &str, key: i64) -> ComponentSpec {
        ComponentSpec::new(ComponentCallSiteId::new(site), typ(name)).keyed(key)
    }

    fn state(value: i64) -> Vec<ComponentStateCell> {
        alloc::vec![ComponentStateCell::new(
            StateFieldId::new(0),
            "count",
            7,
            ComponentValue::Int(value),
        )]
    }

    fn render_bundle() -> ComponentBundle {
        let app_type = typ("App");
        let row_type = typ("Row");
        let mut app_plan = ComponentPlan::new(LocalNodeId::new(0));
        app_plan.nodes.push(
            TemplateNode::element(LocalNodeId::new(0), Primitive::Column)
                .child(LocalNodeId::new(1)),
        );
        app_plan.nodes.push(TemplateNode::element(
            LocalNodeId::new(1),
            Primitive::Fragment,
        ));
        let mut row_plan = ComponentPlan::new(LocalNodeId::new(0));
        row_plan.slots.push(SlotKind::Text);
        row_plan
            .nodes
            .push(TemplateNode::text(LocalNodeId::new(0), ""));
        row_plan
            .updates
            .push(UpdateSite::text(SlotId::new(0), LocalNodeId::new(0)));
        let empty_interface = ComponentInterface {
            props_arity: 0,
            props_type_fingerprint: 1,
            child_contract_fingerprint: 0,
            slot_contract_fingerprint: 0,
        };
        ComponentBundle {
            abi_version: COMPONENT_BUNDLE_ABI_VERSION,
            module_identity: "github.com/acme/app".to_string(),
            root: app_type.clone(),
            linked_modules: Vec::new(),
            definitions: alloc::vec![
                ComponentDefinition {
                    type_id: app_type,
                    display_name: "App".to_string(),
                    mode: ExecutionMode::RootFallback,
                    interface: empty_interface,
                    plan: app_plan.validate(PlanLimits::default()).unwrap(),
                    call_sites: alloc::vec![ComponentCallSite {
                        id: ComponentCallSiteId::new(1),
                        mode: ComponentCallMode::Dynamic,
                        callee: None,
                        mount_parent: LocalNodeId::new(1),
                        mount_before: None,
                        props_bindings: Vec::new(),
                        key_binding: None,
                    }],
                    states: Vec::new(),
                    bindings: Vec::new(),
                    handlers: Vec::new(),
                    effects: Vec::new(),
                    tasks: Vec::new(),
                    lifecycle: LifecycleDefinition::empty(),
                    reload_schema_fingerprint: 1,
                },
                ComponentDefinition {
                    type_id: row_type,
                    display_name: "Row".to_string(),
                    mode: ExecutionMode::RootFallback,
                    interface: empty_interface,
                    plan: row_plan.validate(PlanLimits::default()).unwrap(),
                    call_sites: Vec::new(),
                    states: alloc::vec![StateFieldDefinition {
                        id: StateFieldId::new(0),
                        key: "count".to_string(),
                        type_fingerprint: 7,
                        value_kind: vo_ui_artifact::StateValueKind::Int,
                        has_initializer: true,
                        initializer_func: None,
                        initializer_dependencies: Vec::new(),
                        initializer_props: Vec::new(),
                    }],
                    bindings: alloc::vec![BindingDefinition {
                        id: BindingId::new(0),
                        evaluator_func: None,
                        slots: alloc::vec![SlotId::new(0)],
                        dependencies: Vec::new(),
                        prop_dependencies: Vec::new(),
                    }],
                    handlers: Vec::new(),
                    effects: Vec::new(),
                    tasks: Vec::new(),
                    lifecycle: LifecycleDefinition::empty(),
                    reload_schema_fingerprint: 1,
                },
            ],
            imports: Vec::new(),
            capabilities: alloc::vec!["ui.component-v2".to_string()],
            source: BundleSourceMetadata {
                source_digest: [1; 32],
                compiler_identity: "test-compiler".to_string(),
                reload_schema_version: 1,
            },
        }
    }

    #[derive(Clone, Debug)]
    struct AtomicRenderer {
        tree: TreeMirror,
        reject_next: bool,
    }

    impl AtomicRenderer {
        fn new(epoch: u64, root: NodeId) -> Self {
            Self {
                tree: TreeMirror::new(epoch, root, ProtocolLimits::default()),
                reject_next: false,
            }
        }

        fn node(&self, id: NodeId) -> Option<NodeSnapshot> {
            self.tree.node(id)
        }
    }

    impl Renderer for AtomicRenderer {
        type Error = ApplyError;

        fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
            if core::mem::take(&mut self.reject_next) {
                return Err(ApplyError::MutationLimitExceeded);
            }
            self.tree.apply(batch)
        }
    }

    fn render_child(key: i64, text: &str) -> ComponentSpec {
        child(1, "Row", key)
            .state(state(0))
            .slots([SlotValue::Text(text.to_string())])
    }

    #[test]
    fn keyed_insert_move_remove_and_replace_preserve_exact_instances() {
        let mut forest = ComponentForest::default();
        let mounted = forest
            .mount(
                typ("App"),
                alloc::vec![child(1, "Row", 1), child(1, "Row", 2), child(1, "Row", 3),],
            )
            .unwrap();
        let root = forest.root().unwrap();
        let one = mounted.created[1];
        let two = mounted.created[2];
        let three = mounted.created[3];

        let commit = forest
            .reconcile(alloc::vec![
                child(1, "Row", 3),
                child(1, "Row", 1),
                child(1, "Card", 2),
                child(1, "Row", 4),
            ])
            .unwrap();
        assert_eq!(forest.get(root).unwrap().children[0], three);
        assert_eq!(forest.get(root).unwrap().children[1], one);
        assert_eq!(commit.reused, alloc::vec![three, one]);
        assert_eq!(commit.moved, alloc::vec![three, one]);
        assert_eq!(commit.replacements.len(), 1);
        assert_eq!(commit.replacements[0].previous, two);
        assert_eq!(commit.disposed, alloc::vec![two]);
        assert!(!forest.contains(two));
        assert_eq!(forest.live_count(), 5);
    }

    #[test]
    fn removal_is_postorder_and_reused_slots_advance_generation() {
        let mut forest = ComponentForest::default();
        let mounted = forest
            .mount(
                typ("App"),
                alloc::vec![
                    ComponentSpec::new(ComponentCallSiteId::new(1), typ("Parent"))
                        .children([child(2, "Leaf", 7)])
                ],
            )
            .unwrap();
        let parent = mounted.created[1];
        let leaf = mounted.created[2];
        let removed = forest.reconcile(Vec::new()).unwrap();
        assert_eq!(removed.disposed, alloc::vec![leaf, parent]);
        assert!(!forest.contains(parent));
        assert!(!forest.contains(leaf));

        let next = forest
            .reconcile(alloc::vec![child(1, "Parent", 9)])
            .unwrap();
        let replacement = next.created[0];
        assert!(replacement.index() == parent.index() || replacement.index() == leaf.index());
        assert_ne!(replacement.generation(), parent.generation());
        assert_ne!(replacement.generation(), leaf.generation());
    }

    #[test]
    fn invalid_candidate_rolls_back_every_observable_field() {
        let mut forest = ComponentForest::default();
        forest
            .mount(typ("App"), alloc::vec![child(1, "Row", 1)])
            .unwrap();
        let before = forest.clone();
        let duplicate = forest.reconcile(alloc::vec![child(1, "Row", 2), child(1, "Row", 2),]);
        assert_eq!(duplicate, Err(ComponentForestError::DuplicateKey(2.into())));
        assert_eq!(forest.revision(), before.revision());
        assert_eq!(forest.root(), before.root());
        assert_eq!(forest.live_count(), before.live_count());
        let root = forest.root().unwrap();
        assert_eq!(forest.get(root), before.get(root));
    }

    #[test]
    fn unkeyed_identity_is_call_site_and_stale_parents_are_rejected() {
        let mut forest = ComponentForest::default();
        forest.mount(typ("App"), Vec::new()).unwrap();
        let root = forest.root().unwrap();
        let duplicate = forest.reconcile(alloc::vec![
            ComponentSpec::new(ComponentCallSiteId::new(4), typ("A")),
            ComponentSpec::new(ComponentCallSiteId::new(4), typ("A")),
        ]);
        assert_eq!(
            duplicate,
            Err(ComponentForestError::DuplicateCallSite(
                ComponentCallSiteId::new(4)
            ))
        );
        forest.unmount().unwrap();
        assert_eq!(
            forest.reconcile_children(root, Vec::new()),
            Err(ComponentForestError::StaleInstance(root))
        );
    }

    #[test]
    fn equal_component_types_own_independent_structured_state() {
        let mut forest = ComponentForest::default();
        let mounted = forest
            .mount(
                typ("App"),
                alloc::vec![
                    child(1, "Row", 1).state(state(10)),
                    child(1, "Row", 2).state(state(20)),
                ],
            )
            .unwrap();
        let first = mounted.created[1];
        let second = mounted.created[2];
        let write = forest
            .write_state(
                first,
                alloc::vec![ComponentStateWrite::new(
                    StateFieldId::new(0),
                    7,
                    ComponentValue::Record(BTreeMap::from([(
                        "nested".into(),
                        ComponentValue::List(alloc::vec![ComponentValue::Int(11)]),
                    )])),
                )],
            )
            .unwrap();
        assert_eq!(write.changed, alloc::vec![StateFieldId::new(0)]);
        assert!(matches!(
            forest.get(first).unwrap().state[0].value,
            ComponentValue::Record(_)
        ));
        assert_eq!(
            forest.get(second).unwrap().state[0].value,
            ComponentValue::Int(20)
        );
    }

    #[test]
    fn state_transactions_and_handler_lifetimes_are_generation_checked() {
        let handler_site = HandlerSiteId::new(0);
        let mut forest = ComponentForest::default();
        let mounted = forest
            .mount(
                typ("App"),
                alloc::vec![child(1, "Row", 1).state(state(1)).handlers([handler_site])],
            )
            .unwrap();
        let row = mounted.created[1];
        let handler = forest.handler(row, handler_site).unwrap();
        let revision = forest.revision();
        assert_eq!(
            forest.write_state(
                row,
                alloc::vec![ComponentStateWrite::new(
                    StateFieldId::new(0),
                    99,
                    ComponentValue::Int(2),
                )],
            ),
            Err(ComponentForestError::StateTypeMismatch(StateFieldId::new(
                0
            )))
        );
        assert_eq!(forest.revision(), revision);
        assert_eq!(
            forest.get(row).unwrap().state[0].value,
            ComponentValue::Int(1)
        );

        forest.reconcile(Vec::new()).unwrap();
        assert!(forest.resolve_handler(handler).is_none());
        assert!(!forest.contains(row));
    }

    #[test]
    fn component_and_renderer_forests_reorder_atomically_with_stable_nodes() {
        let root = NodeId::new(0, 1);
        let renderer = AtomicRenderer::new(21, root);
        let mut runtime = ComponentTemplateRuntime::new(
            renderer,
            21,
            root,
            render_bundle(),
            BundleLimits::default(),
            ComponentForestLimits::default(),
        )
        .unwrap();
        runtime
            .mount(
                Vec::new(),
                Vec::new(),
                Vec::new(),
                alloc::vec![render_child(1, "A"), render_child(2, "B")],
            )
            .unwrap();
        let app = runtime.renderer().node(root).unwrap().children[0];
        let anchor = runtime.renderer().node(app).unwrap().children[0];
        let before = runtime.renderer().node(anchor).unwrap().children;

        let commit = runtime
            .reconcile(alloc::vec![render_child(2, "B2"), render_child(1, "A"),])
            .unwrap();
        assert_eq!(commit.forest.created.len(), 0);
        assert_eq!(commit.forest.moved.len(), 2);
        let after = runtime.renderer().node(anchor).unwrap().children;
        assert_eq!(after, alloc::vec![before[1], before[0]]);
        assert_eq!(runtime.renderer().node(before[1]).unwrap().text, "B2");
        assert_eq!(runtime.renderer().node(before[0]).unwrap().text, "A");
    }

    #[test]
    fn renderer_rejection_rolls_back_component_candidate_and_node_tree() {
        let root = NodeId::new(0, 1);
        let renderer = AtomicRenderer::new(22, root);
        let mut runtime = ComponentTemplateRuntime::new(
            renderer,
            22,
            root,
            render_bundle(),
            BundleLimits::default(),
            ComponentForestLimits::default(),
        )
        .unwrap();
        runtime
            .mount(
                Vec::new(),
                Vec::new(),
                Vec::new(),
                alloc::vec![render_child(1, "A")],
            )
            .unwrap();
        let revision = runtime.revision();
        let live = runtime.forest().live_count();
        let app = runtime.renderer().node(root).unwrap().children[0];
        let anchor = runtime.renderer().node(app).unwrap().children[0];
        let children = runtime.renderer().node(anchor).unwrap().children;
        runtime.renderer_mut().reject_next = true;
        assert!(matches!(
            runtime.reconcile(alloc::vec![
                render_child(1, "changed"),
                render_child(2, "new"),
            ]),
            Err(ComponentRenderError::Renderer(
                ApplyError::MutationLimitExceeded
            ))
        ));
        assert_eq!(runtime.revision(), revision);
        assert_eq!(runtime.forest().live_count(), live);
        assert_eq!(runtime.renderer().node(anchor).unwrap().children, children);
        assert_eq!(runtime.renderer().node(children[0]).unwrap().text, "A");
    }

    #[test]
    fn reload_preserves_compatible_instance_state_and_rolls_back_failure() {
        let root = NodeId::new(0, 1);
        let renderer = AtomicRenderer::new(23, root);
        let mut runtime = ComponentTemplateRuntime::new(
            renderer,
            23,
            root,
            render_bundle(),
            BundleLimits::default(),
            ComponentForestLimits::default(),
        )
        .unwrap();
        runtime
            .mount(
                Vec::new(),
                Vec::new(),
                Vec::new(),
                alloc::vec![render_child(1, "A")],
            )
            .unwrap();
        let component_root = runtime.forest().root().unwrap();
        let row = runtime.forest().get(component_root).unwrap().children[0];
        runtime
            .write_state(
                row,
                alloc::vec![ComponentStateWrite::new(
                    StateFieldId::new(0),
                    7,
                    ComponentValue::Int(42),
                )],
            )
            .unwrap();
        let app_node = runtime.renderer().node(root).unwrap().children[0];
        let anchor = runtime.renderer().node(app_node).unwrap().children[0];
        let old_row_node = runtime.renderer().node(anchor).unwrap().children[0];

        let mut next_bundle = render_bundle();
        next_bundle.source.source_digest = [2; 32];
        runtime
            .reload_bundle(
                next_bundle.clone(),
                BundleLimits::default(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                alloc::vec![render_child(1, "A2")],
            )
            .unwrap();
        let row_after = runtime.forest().get(component_root).unwrap().children[0];
        assert_eq!(row_after, row);
        assert_eq!(
            runtime.forest().get(row).unwrap().state[0].value,
            ComponentValue::Int(42)
        );
        let app_node = runtime.renderer().node(root).unwrap().children[0];
        let anchor = runtime.renderer().node(app_node).unwrap().children[0];
        let new_row_node = runtime.renderer().node(anchor).unwrap().children[0];
        assert_ne!(new_row_node, old_row_node);
        assert_eq!(runtime.renderer().node(new_row_node).unwrap().text, "A2");

        let revision = runtime.revision();
        runtime.renderer_mut().reject_next = true;
        assert!(matches!(
            runtime.reload_bundle(
                render_bundle(),
                BundleLimits::default(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                alloc::vec![render_child(1, "lost")],
            ),
            Err(ComponentRenderError::Renderer(
                ApplyError::MutationLimitExceeded
            ))
        ));
        assert_eq!(runtime.revision(), revision);
        assert_eq!(
            runtime.forest().get(row).unwrap().state[0].value,
            ComponentValue::Int(42)
        );
        assert_eq!(runtime.renderer().node(new_row_node).unwrap().text, "A2");
    }
}
