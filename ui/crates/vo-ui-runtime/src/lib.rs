#![no_std]

extern crate alloc;
#[cfg(test)]
extern crate std;

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{EventType, Key, Listener, NodeId, Property, PropertyId, View, ViewKind};
use vo_ui_plan::{
    DirectMutation, LocalNodeId, SlotId, SlotKind, SlotValue, TemplateNodeKind, ValidatedPlan,
};
use vo_ui_protocol::{Mutation, MutationBatch, NodeKind, Renderer};

mod component;

pub use component::{
    ComponentForest, ComponentForestCommit, ComponentForestError, ComponentForestLimits,
    ComponentHandlerId, ComponentInstanceId, ComponentRenderCommit, ComponentRenderError,
    ComponentReplacement, ComponentSpec, ComponentStateCell, ComponentStateCommit,
    ComponentStateWrite, ComponentTemplateRuntime, ComponentValue, MountedComponent,
};

#[derive(Debug)]
pub enum ReconcileError<E> {
    Renderer(E),
    DuplicateKey(Key),
    NodeIdentityExhausted,
}

impl<E: fmt::Display> fmt::Display for ReconcileError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Renderer(error) => write!(formatter, "renderer rejected UI revision: {error}"),
            Self::DuplicateKey(key) => write!(formatter, "duplicate sibling key: {key:?}"),
            Self::NodeIdentityExhausted => {
                formatter.write_str("UI node identity space is exhausted")
            }
        }
    }
}

#[derive(Clone, Debug)]
struct NodeAllocator {
    generations: Vec<u32>,
    free: Vec<u32>,
}

impl NodeAllocator {
    fn with_reserved(root: NodeId) -> Self {
        let mut generations = alloc::vec![1; root.index() as usize + 1];
        generations[root.index() as usize] = root.generation();
        Self {
            generations,
            free: Vec::new(),
        }
    }

    fn allocate(&mut self) -> Result<NodeId, ()> {
        if let Some(index) = self.free.pop() {
            return Ok(NodeId::new(index, self.generations[index as usize]));
        }
        let index = u32::try_from(self.generations.len()).map_err(|_| ())?;
        self.generations.push(1);
        Ok(NodeId::new(index, 1))
    }

    fn release(&mut self, id: NodeId) {
        let generation = &mut self.generations[id.index() as usize];
        *generation = generation.wrapping_add(1).max(1);
        self.free.push(id.index());
    }
}

#[derive(Clone, Debug)]
struct MountedNode {
    id: NodeId,
    key: Option<Key>,
    kind: MountedKind,
    properties: BTreeMap<PropertyId, vo_ui_core::Value>,
    listeners: BTreeMap<EventType, Listener>,
    children: Vec<MountedNode>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum MountedKind {
    Element(vo_ui_core::Primitive),
    Text(String),
}

impl MountedKind {
    fn protocol_kind(&self) -> NodeKind {
        match self {
            Self::Element(primitive) => NodeKind::Element(*primitive),
            Self::Text(_) => NodeKind::Text,
        }
    }

    fn compatible(&self, view: &ViewKind) -> bool {
        match (self, view) {
            (Self::Element(current), ViewKind::Element(next)) => current == next,
            (Self::Text(_), ViewKind::Text(_)) => true,
            _ => false,
        }
    }
}

pub struct UiRuntime<R: Renderer> {
    renderer: R,
    root: NodeId,
    session_epoch: u64,
    revision: u64,
    allocator: NodeAllocator,
    children: Vec<MountedNode>,
    last_batch: Option<MutationBatch>,
}

impl<R: Renderer> UiRuntime<R> {
    pub fn new(renderer: R, session_epoch: u64, root: NodeId) -> Self {
        Self {
            renderer,
            root,
            session_epoch,
            revision: 0,
            allocator: NodeAllocator::with_reserved(root),
            children: Vec::new(),
            last_batch: None,
        }
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

    pub fn into_renderer(self) -> R {
        self.renderer
    }

    pub fn mount(&mut self, view: View) -> Result<MutationBatch, ReconcileError<R::Error>> {
        self.update_children(alloc::vec![view])
    }

    pub fn update(&mut self, view: View) -> Result<MutationBatch, ReconcileError<R::Error>> {
        self.update_children(alloc::vec![view])
    }

    pub fn update_children(
        &mut self,
        views: Vec<View>,
    ) -> Result<MutationBatch, ReconcileError<R::Error>> {
        let mut allocator = self.allocator.clone();
        let mut children = self.children.clone();
        let mut mutations = Vec::new();
        reconcile_children(
            self.root,
            &mut children,
            views,
            &mut allocator,
            &mut mutations,
        )
        .map_err(|error| match error {
            LocalError::DuplicateKey(key) => ReconcileError::DuplicateKey(key),
            LocalError::NodeIdentityExhausted => ReconcileError::NodeIdentityExhausted,
        })?;
        let batch = MutationBatch::new(
            self.session_epoch,
            self.revision.saturating_add(1),
            mutations,
        );
        self.renderer
            .apply(&batch)
            .map_err(ReconcileError::Renderer)?;
        self.allocator = allocator;
        self.children = children;
        self.revision = batch.revision;
        self.last_batch = Some(batch.clone());
        Ok(batch)
    }
}

impl<R: Renderer> UiRuntime<R> {
    /// Returns the last renderer revision accepted by the platform.
    pub fn last_batch(&self) -> Option<&MutationBatch> {
        self.last_batch.as_ref()
    }
}

#[derive(Debug)]
pub enum TemplateError<E> {
    Renderer(E),
    AlreadyMounted,
    NotMounted,
    SlotCountMismatch {
        expected: usize,
        found: usize,
    },
    InvalidSlot(SlotId),
    SlotKindMismatch {
        slot: SlotId,
        expected: SlotKind,
        found: SlotKind,
    },
    NodeIdentityExhausted,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TemplateProjectionError {
    KeyUnsupported(LocalNodeId),
    NodeKindMismatch(LocalNodeId),
    ChildCountMismatch(LocalNodeId),
    StaticTextMismatch(LocalNodeId),
    StaticPropertyMismatch {
        node: LocalNodeId,
        property: PropertyId,
    },
    ListenerMismatch(LocalNodeId),
    MissingDynamicText(LocalNodeId),
    MissingDynamicProperty {
        node: LocalNodeId,
        property: PropertyId,
    },
    MissingSlotValue(SlotId),
    InconsistentSlotValue(SlotId),
}

impl fmt::Display for TemplateProjectionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "rendered View does not match its compiled component plan: {self:?}"
        )
    }
}

/// Validates one root-fallback render against its immutable template and
/// extracts only the dynamic slot values. This lets development VM/JIT builds
/// use the same O(changed slots) renderer path before every expression has a
/// dedicated evaluator entrypoint.
pub fn project_template_slots(
    plan: &ValidatedPlan,
    view: &View,
) -> Result<Vec<SlotValue>, TemplateProjectionError> {
    let mut views = alloc::vec![None; plan.nodes().len()];
    project_node(plan, plan.root(), view, &mut views)?;

    let mut slots = Vec::with_capacity(plan.slots().len());
    for index in 0..plan.slots().len() {
        let slot = SlotId::new(index as u32);
        let mut projected = None;
        for site in plan
            .update_sites(slot)
            .expect("validated plans index every slot")
        {
            let value = match site.mutation {
                DirectMutation::SetText { target } => {
                    let view = views[target.index() as usize]
                        .expect("template traversal populated every reachable node");
                    let ViewKind::Text(text) = &view.kind else {
                        return Err(TemplateProjectionError::MissingDynamicText(target));
                    };
                    SlotValue::Text(text.clone())
                }
                DirectMutation::SetProperty { target, property } => {
                    let view = views[target.index() as usize]
                        .expect("template traversal populated every reachable node");
                    let value = view
                        .properties
                        .iter()
                        .find(|candidate| candidate.id == property)
                        .map(|candidate| candidate.value.clone())
                        .ok_or(TemplateProjectionError::MissingDynamicProperty {
                            node: target,
                            property,
                        })?;
                    SlotValue::Property(value)
                }
            };
            if projected.as_ref().is_some_and(|current| current != &value) {
                return Err(TemplateProjectionError::InconsistentSlotValue(slot));
            }
            projected = Some(value);
        }
        slots.push(projected.ok_or(TemplateProjectionError::MissingSlotValue(slot))?);
    }
    Ok(slots)
}

fn project_node<'a>(
    plan: &ValidatedPlan,
    id: LocalNodeId,
    view: &'a View,
    views: &mut [Option<&'a View>],
) -> Result<(), TemplateProjectionError> {
    let node = plan.node(id);
    if view.key.is_some() {
        return Err(TemplateProjectionError::KeyUnsupported(id));
    }
    match (&node.kind, &view.kind) {
        (TemplateNodeKind::Element(expected), ViewKind::Element(found)) if expected == found => {}
        (TemplateNodeKind::Text, ViewKind::Text(_)) => {}
        _ => return Err(TemplateProjectionError::NodeKindMismatch(id)),
    }
    if node.children.len() != view.children.len() {
        return Err(TemplateProjectionError::ChildCountMismatch(id));
    }

    match &node.kind {
        TemplateNodeKind::Text => {
            let dynamic = plan.as_plan().updates.iter().any(
                |site| matches!(site.mutation, DirectMutation::SetText { target } if target == id),
            );
            if !dynamic {
                let ViewKind::Text(text) = &view.kind else {
                    unreachable!("node kind was checked")
                };
                if text != &node.text {
                    return Err(TemplateProjectionError::StaticTextMismatch(id));
                }
            }
        }
        TemplateNodeKind::Element(_) => {
            for property in &node.properties {
                if view
                    .properties
                    .iter()
                    .find(|candidate| candidate.id == property.id)
                    != Some(property)
                {
                    return Err(TemplateProjectionError::StaticPropertyMismatch {
                        node: id,
                        property: property.id,
                    });
                }
            }
            for property in &view.properties {
                let is_static = node
                    .properties
                    .iter()
                    .any(|candidate| candidate.id == property.id);
                let is_dynamic = plan.as_plan().updates.iter().any(|site| {
                    matches!(
                        site.mutation,
                        DirectMutation::SetProperty { target, property: id }
                            if target == node.id && id == property.id
                    )
                });
                if !is_static && !is_dynamic {
                    return Err(TemplateProjectionError::StaticPropertyMismatch {
                        node: id,
                        property: property.id,
                    });
                }
            }
            if node.listeners.len() != view.listeners.len()
                || node
                    .listeners
                    .iter()
                    .any(|listener| !view.listeners.iter().any(|candidate| candidate == listener))
            {
                return Err(TemplateProjectionError::ListenerMismatch(id));
            }
        }
    }
    views[id.index() as usize] = Some(view);
    for (child, child_view) in node.children.iter().copied().zip(&view.children) {
        project_node(plan, child, child_view, views)?;
    }
    Ok(())
}

impl<E: fmt::Display> fmt::Display for TemplateError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Renderer(error) => write!(formatter, "renderer rejected UI revision: {error}"),
            Self::AlreadyMounted => formatter.write_str("a compiled component is already mounted"),
            Self::NotMounted => formatter.write_str("no compiled component is mounted"),
            Self::SlotCountMismatch { expected, found } => {
                write!(
                    formatter,
                    "component expected {expected} slots and received {found}"
                )
            }
            Self::InvalidSlot(slot) => {
                write!(formatter, "component slot {} is invalid", slot.index())
            }
            Self::SlotKindMismatch {
                slot,
                expected,
                found,
            } => write!(
                formatter,
                "component slot {} expects {expected:?} and received {found:?}",
                slot.index()
            ),
            Self::NodeIdentityExhausted => {
                formatter.write_str("UI node identity space is exhausted")
            }
        }
    }
}

#[derive(Clone, Debug)]
struct MountedTemplate {
    plan: ValidatedPlan,
    nodes: Vec<NodeId>,
    slots: Vec<SlotValue>,
}

/// Executes compiler-generated component plans without rebuilding or diffing a
/// `View`. One changed slot visits only its pre-indexed update sites.
pub struct TemplateRuntime<R: Renderer> {
    renderer: R,
    root: NodeId,
    session_epoch: u64,
    revision: u64,
    allocator: NodeAllocator,
    mounted: Option<MountedTemplate>,
    last_batch: Option<MutationBatch>,
    next_batch: MutationBatch,
    pending_slots: Vec<Option<SlotValue>>,
    pending_indices: Vec<SlotId>,
}

impl<R: Renderer> TemplateRuntime<R> {
    pub fn new(renderer: R, session_epoch: u64, root: NodeId) -> Self {
        Self {
            renderer,
            root,
            session_epoch,
            revision: 0,
            allocator: NodeAllocator::with_reserved(root),
            mounted: None,
            last_batch: None,
            next_batch: MutationBatch::new(session_epoch, 0, Vec::new()),
            pending_slots: Vec::new(),
            pending_indices: Vec::new(),
        }
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

    pub fn into_renderer(self) -> R {
        self.renderer
    }

    pub fn last_batch(&self) -> Option<&MutationBatch> {
        self.last_batch.as_ref()
    }

    pub fn mount(
        &mut self,
        plan: ValidatedPlan,
        slots: Vec<SlotValue>,
    ) -> Result<MutationBatch, TemplateError<R::Error>> {
        if self.mounted.is_some() {
            return Err(TemplateError::AlreadyMounted);
        }
        validate_slot_values(&plan, &slots)?;

        let mut allocator = self.allocator.clone();
        let (nodes, mut mutations) = prepare_template(&plan, &slots, &mut allocator)?;
        mutations.push(Mutation::InsertBefore {
            parent: self.root,
            child: nodes[plan.root().index() as usize],
            before: None,
        });

        let batch = MutationBatch::new(
            self.session_epoch,
            self.revision.saturating_add(1),
            mutations,
        );
        self.renderer
            .apply(&batch)
            .map_err(TemplateError::Renderer)?;
        self.allocator = allocator;
        self.prepare_update_buffers(&plan, batch.mutations.len());
        self.mounted = Some(MountedTemplate { plan, nodes, slots });
        self.revision = batch.revision;
        self.last_batch = Some(batch.clone());
        Ok(batch)
    }

    /// Atomically replaces the mounted template. Hosts apply one revision, so
    /// a development reload cannot expose a partially torn-down component.
    pub fn replace(
        &mut self,
        plan: ValidatedPlan,
        slots: Vec<SlotValue>,
    ) -> Result<MutationBatch, TemplateError<R::Error>> {
        let mounted = self.mounted.as_ref().ok_or(TemplateError::NotMounted)?;
        validate_slot_values(&plan, &slots)?;

        let mut allocator = self.allocator.clone();
        let (nodes, mut mutations) = prepare_template(&plan, &slots, &mut allocator)?;
        let previous_root = mounted.nodes[mounted.plan.root().index() as usize];
        mutations.push(Mutation::InsertBefore {
            parent: self.root,
            child: nodes[plan.root().index() as usize],
            before: Some(previous_root),
        });
        emit_template_detach(self.root, mounted, &mut mutations);

        let batch = MutationBatch::new(
            self.session_epoch,
            self.revision.saturating_add(1),
            mutations,
        );
        self.renderer
            .apply(&batch)
            .map_err(TemplateError::Renderer)?;
        for id in mounted.nodes.iter().rev().copied() {
            allocator.release(id);
        }
        self.allocator = allocator;
        self.prepare_update_buffers(&plan, batch.mutations.len());
        self.mounted = Some(MountedTemplate { plan, nodes, slots });
        self.revision = batch.revision;
        self.last_batch = Some(batch.clone());
        Ok(batch)
    }

    pub fn update_slot(
        &mut self,
        slot: SlotId,
        value: SlotValue,
    ) -> Result<Option<MutationBatch>, TemplateError<R::Error>> {
        self.update_slots_in_place(core::iter::once((slot, value)))
            .map(|batch| batch.cloned())
    }

    /// Coalesces repeated writes to the same slot and commits all changed slots
    /// as one renderer revision.
    pub fn update_slots(
        &mut self,
        updates: impl IntoIterator<Item = (SlotId, SlotValue)>,
    ) -> Result<Option<MutationBatch>, TemplateError<R::Error>> {
        self.update_slots_in_place(updates)
            .map(|batch| batch.cloned())
    }

    /// Applies changed slots through retained scratch buffers and returns the
    /// accepted batch by reference. Compiled runtimes use this path so scalar
    /// updates allocate no host memory after mount-time preallocation.
    pub fn update_slots_in_place(
        &mut self,
        updates: impl IntoIterator<Item = (SlotId, SlotValue)>,
    ) -> Result<Option<&MutationBatch>, TemplateError<R::Error>> {
        let slot_count = self
            .mounted
            .as_ref()
            .ok_or(TemplateError::NotMounted)?
            .plan
            .slots()
            .len();
        debug_assert_eq!(self.pending_slots.len(), slot_count);
        self.clear_pending_slots();
        for (slot, value) in updates {
            let Some(expected) = self
                .mounted
                .as_ref()
                .expect("mounted component was checked")
                .plan
                .slot_kind(slot)
            else {
                self.clear_pending_slots();
                return Err(TemplateError::InvalidSlot(slot));
            };
            let found = value.kind();
            if found != expected {
                self.clear_pending_slots();
                return Err(TemplateError::SlotKindMismatch {
                    slot,
                    expected,
                    found,
                });
            }
            let pending = &mut self.pending_slots[slot.index() as usize];
            if pending.is_none() {
                self.pending_indices.push(slot);
            }
            *pending = Some(value);
        }
        self.pending_indices.sort_unstable();
        let mut changed_len = 0;
        for index in 0..self.pending_indices.len() {
            let slot = self.pending_indices[index];
            let changed = {
                let mounted = self
                    .mounted
                    .as_ref()
                    .expect("mounted component was checked");
                self.pending_slots[slot.index() as usize]
                    .as_ref()
                    .is_some_and(|value| mounted.slots.get(slot.index() as usize) != Some(value))
            };
            if changed {
                self.pending_indices[changed_len] = slot;
                changed_len += 1;
            } else {
                self.pending_slots[slot.index() as usize] = None;
            }
        }
        self.pending_indices.truncate(changed_len);
        if self.pending_indices.is_empty() {
            self.clear_pending_slots();
            return Ok(None);
        }

        self.next_batch.session_epoch = self.session_epoch;
        self.next_batch.revision = self.revision.saturating_add(1);
        self.next_batch.mutations.clear();
        let mounted = self
            .mounted
            .as_ref()
            .expect("mounted component was checked");
        for slot in &self.pending_indices {
            let value = self.pending_slots[slot.index() as usize]
                .as_ref()
                .expect("pending slot index has a value");
            emit_slot_mutations(
                &mounted.plan,
                &mounted.nodes,
                *slot,
                value,
                &mut self.next_batch.mutations,
            );
        }
        if let Err(error) = self.renderer.apply(&self.next_batch) {
            self.clear_pending_slots();
            return Err(TemplateError::Renderer(error));
        }
        let mounted = self
            .mounted
            .as_mut()
            .expect("mounted component was checked");
        for slot in self.pending_indices.drain(..) {
            mounted.slots[slot.index() as usize] = self.pending_slots[slot.index() as usize]
                .take()
                .expect("accepted pending slot has a value");
        }
        self.revision = self.next_batch.revision;
        let mut accepted = self
            .last_batch
            .take()
            .expect("mounted templates retain their accepted mount batch");
        core::mem::swap(&mut accepted, &mut self.next_batch);
        self.last_batch = Some(accepted);
        Ok(self.last_batch.as_ref())
    }

    pub fn unmount(&mut self) -> Result<MutationBatch, TemplateError<R::Error>> {
        let mounted = self.mounted.as_ref().ok_or(TemplateError::NotMounted)?;
        let mut mutations = Vec::new();
        emit_template_detach(self.root, mounted, &mut mutations);
        let batch = MutationBatch::new(
            self.session_epoch,
            self.revision.saturating_add(1),
            mutations,
        );
        self.renderer
            .apply(&batch)
            .map_err(TemplateError::Renderer)?;

        let mut allocator = self.allocator.clone();
        for id in mounted.nodes.iter().rev().copied() {
            allocator.release(id);
        }
        self.allocator = allocator;
        self.mounted = None;
        self.pending_slots.clear();
        self.pending_indices.clear();
        self.revision = batch.revision;
        self.last_batch = Some(batch.clone());
        Ok(batch)
    }

    fn prepare_update_buffers(&mut self, plan: &ValidatedPlan, mount_mutations: usize) {
        self.pending_slots.clear();
        self.pending_slots.resize_with(plan.slots().len(), || None);
        self.pending_indices.clear();
        self.pending_indices.reserve(
            plan.slots()
                .len()
                .saturating_sub(self.pending_indices.capacity()),
        );
        let required_mutations = plan.as_plan().updates.len().max(mount_mutations);
        self.next_batch.mutations.clear();
        self.next_batch.mutations.next_reserve(required_mutations);
    }

    fn clear_pending_slots(&mut self) {
        for slot in self.pending_indices.drain(..) {
            self.pending_slots[slot.index() as usize] = None;
        }
    }
}

trait MutationBatchBuffer {
    fn next_reserve(&mut self, required: usize);
}

impl MutationBatchBuffer for Vec<Mutation> {
    fn next_reserve(&mut self, required: usize) {
        if self.capacity() < required {
            self.reserve(required - self.capacity());
        }
    }
}

fn prepare_template<E>(
    plan: &ValidatedPlan,
    slots: &[SlotValue],
    allocator: &mut NodeAllocator,
) -> Result<(Vec<NodeId>, Vec<Mutation>), TemplateError<E>> {
    let mut nodes = Vec::with_capacity(plan.nodes().len());
    for _ in plan.nodes() {
        nodes.push(
            allocator
                .allocate()
                .map_err(|()| TemplateError::NodeIdentityExhausted)?,
        );
    }

    let mut mutations = Vec::new();
    for node in plan.nodes() {
        let id = nodes[node.id.index() as usize];
        let kind = match node.kind {
            TemplateNodeKind::Element(primitive) => NodeKind::Element(primitive),
            TemplateNodeKind::Text => NodeKind::Text,
        };
        mutations.push(Mutation::Create { id, kind });
        match node.kind {
            TemplateNodeKind::Text if !node.text.is_empty() => {
                mutations.push(Mutation::SetText {
                    id,
                    text: node.text.clone(),
                });
            }
            TemplateNodeKind::Text => {}
            TemplateNodeKind::Element(_) => {
                mutations.extend(
                    node.properties
                        .iter()
                        .cloned()
                        .map(|property| Mutation::SetProperty { id, property }),
                );
                mutations.extend(
                    node.listeners
                        .iter()
                        .copied()
                        .map(|listener| Mutation::Listen { id, listener }),
                );
            }
        }
    }
    for (index, value) in slots.iter().enumerate() {
        emit_slot_mutations(
            plan,
            &nodes,
            SlotId::new(index as u32),
            value,
            &mut mutations,
        );
    }
    for node in plan.nodes() {
        let parent = nodes[node.id.index() as usize];
        for child in &node.children {
            mutations.push(Mutation::InsertBefore {
                parent,
                child: nodes[child.index() as usize],
                before: None,
            });
        }
    }
    Ok((nodes, mutations))
}

fn emit_template_detach(root: NodeId, mounted: &MountedTemplate, mutations: &mut Vec<Mutation>) {
    mutations.push(Mutation::Remove {
        parent: root,
        child: mounted.nodes[mounted.plan.root().index() as usize],
    });
    for node in mounted.plan.nodes() {
        let parent = mounted.nodes[node.id.index() as usize];
        mutations.extend(node.children.iter().map(|child| Mutation::Remove {
            parent,
            child: mounted.nodes[child.index() as usize],
        }));
    }
    mutations.extend(
        mounted
            .nodes
            .iter()
            .rev()
            .copied()
            .map(|id| Mutation::Delete { id }),
    );
}

fn validate_slot_values<E>(
    plan: &ValidatedPlan,
    slots: &[SlotValue],
) -> Result<(), TemplateError<E>> {
    if slots.len() != plan.slots().len() {
        return Err(TemplateError::SlotCountMismatch {
            expected: plan.slots().len(),
            found: slots.len(),
        });
    }
    for (index, value) in slots.iter().enumerate() {
        let slot = SlotId::new(index as u32);
        let expected = plan.slots()[index];
        let found = value.kind();
        if expected != found {
            return Err(TemplateError::SlotKindMismatch {
                slot,
                expected,
                found,
            });
        }
    }
    Ok(())
}

fn emit_slot_mutations(
    plan: &ValidatedPlan,
    nodes: &[NodeId],
    slot: SlotId,
    value: &SlotValue,
    mutations: &mut Vec<Mutation>,
) {
    let sites = plan
        .update_sites(slot)
        .expect("slot was validated against the component plan");
    for site in sites {
        match (site.mutation, value) {
            (DirectMutation::SetText { target }, SlotValue::Text(text)) => {
                mutations.push(Mutation::SetText {
                    id: nodes[target.index() as usize],
                    text: text.clone(),
                });
            }
            (DirectMutation::SetProperty { target, property }, SlotValue::Property(value)) => {
                mutations.push(Mutation::SetProperty {
                    id: nodes[target.index() as usize],
                    property: Property {
                        id: property,
                        value: value.clone(),
                    },
                });
            }
            _ => unreachable!("validated plans preserve slot/update kinds"),
        }
    }
}

#[derive(Debug)]
enum LocalError {
    DuplicateKey(Key),
    NodeIdentityExhausted,
}

fn reconcile_children(
    parent: NodeId,
    mounted: &mut Vec<MountedNode>,
    views: Vec<View>,
    allocator: &mut NodeAllocator,
    mutations: &mut Vec<Mutation>,
) -> Result<(), LocalError> {
    ensure_unique_keys(&views)?;
    let previous_order = mounted.iter().map(|node| node.id).collect::<Vec<_>>();
    let mut old = core::mem::take(mounted)
        .into_iter()
        .map(Some)
        .collect::<Vec<_>>();
    let mut next = Vec::with_capacity(views.len());

    for (new_index, view) in views.into_iter().enumerate() {
        let matched = match &view.key {
            Some(key) => old.iter().position(|node| {
                node.as_ref()
                    .is_some_and(|node| node.key.as_ref() == Some(key))
            }),
            None => old.get(new_index).and_then(|node| {
                node.as_ref()
                    .filter(|node| node.key.is_none())
                    .map(|_| new_index)
            }),
        };
        let node = if let Some(index) = matched {
            let current = old[index].take().expect("matched mounted node exists");
            if current.kind.compatible(&view.kind) {
                reconcile_node(current, view, allocator, mutations)?
            } else {
                unmount_child(parent, current, allocator, mutations);
                mount_node(view, allocator, mutations)?
            }
        } else {
            mount_node(view, allocator, mutations)?
        };
        next.push(node);
    }

    for node in old.into_iter().flatten() {
        unmount_child(parent, node, allocator, mutations);
    }
    let remaining_order = previous_order
        .into_iter()
        .filter(|id| next.iter().any(|node| node.id == *id))
        .collect::<Vec<_>>();
    let desired_order = next.iter().map(|node| node.id).collect::<Vec<_>>();
    emit_order_mutations(parent, remaining_order, &desired_order, mutations);
    *mounted = next;
    Ok(())
}

fn reconcile_node(
    mut mounted: MountedNode,
    view: View,
    allocator: &mut NodeAllocator,
    mutations: &mut Vec<Mutation>,
) -> Result<MountedNode, LocalError> {
    mounted.key = view.key;
    match (&mut mounted.kind, view.kind) {
        (MountedKind::Text(current), ViewKind::Text(next)) => {
            if *current != next {
                mutations.push(Mutation::SetText {
                    id: mounted.id,
                    text: next.clone(),
                });
                *current = next;
            }
        }
        (MountedKind::Element(_), ViewKind::Element(_)) => {
            reconcile_properties(
                mounted.id,
                &mut mounted.properties,
                view.properties,
                mutations,
            );
            reconcile_listeners(
                mounted.id,
                &mut mounted.listeners,
                view.listeners,
                mutations,
            );
            reconcile_children(
                mounted.id,
                &mut mounted.children,
                view.children,
                allocator,
                mutations,
            )?;
        }
        _ => unreachable!("caller checks mounted/view compatibility"),
    }
    Ok(mounted)
}

fn mount_node(
    view: View,
    allocator: &mut NodeAllocator,
    mutations: &mut Vec<Mutation>,
) -> Result<MountedNode, LocalError> {
    let id = allocator
        .allocate()
        .map_err(|_| LocalError::NodeIdentityExhausted)?;
    let kind = match &view.kind {
        ViewKind::Element(primitive) => MountedKind::Element(*primitive),
        ViewKind::Text(text) => MountedKind::Text(text.clone()),
    };
    mutations.push(Mutation::Create {
        id,
        kind: kind.protocol_kind(),
    });
    let mut mounted = MountedNode {
        id,
        key: view.key,
        kind,
        properties: BTreeMap::new(),
        listeners: BTreeMap::new(),
        children: Vec::new(),
    };
    match view.kind {
        ViewKind::Text(text) => mutations.push(Mutation::SetText { id, text }),
        ViewKind::Element(_) => {
            reconcile_properties(id, &mut mounted.properties, view.properties, mutations);
            reconcile_listeners(id, &mut mounted.listeners, view.listeners, mutations);
            reconcile_children(
                id,
                &mut mounted.children,
                view.children,
                allocator,
                mutations,
            )?;
        }
    }
    Ok(mounted)
}

fn unmount_child(
    parent: NodeId,
    node: MountedNode,
    allocator: &mut NodeAllocator,
    mutations: &mut Vec<Mutation>,
) {
    mutations.push(Mutation::Remove {
        parent,
        child: node.id,
    });
    unmount_detached(node, allocator, mutations);
}

fn unmount_detached(
    node: MountedNode,
    allocator: &mut NodeAllocator,
    mutations: &mut Vec<Mutation>,
) {
    for child in node.children {
        mutations.push(Mutation::Remove {
            parent: node.id,
            child: child.id,
        });
        unmount_detached(child, allocator, mutations);
    }
    mutations.push(Mutation::Delete { id: node.id });
    allocator.release(node.id);
}

fn reconcile_properties(
    id: NodeId,
    mounted: &mut BTreeMap<PropertyId, vo_ui_core::Value>,
    properties: Vec<Property>,
    mutations: &mut Vec<Mutation>,
) {
    let next = properties
        .into_iter()
        .map(|property| (property.id, property.value))
        .collect::<BTreeMap<_, _>>();
    for property in mounted.keys().filter(|id| !next.contains_key(id)) {
        mutations.push(Mutation::RemoveProperty {
            id,
            property: *property,
        });
    }
    for (property, value) in &next {
        if mounted.get(property) != Some(value) {
            mutations.push(Mutation::SetProperty {
                id,
                property: Property {
                    id: *property,
                    value: value.clone(),
                },
            });
        }
    }
    *mounted = next;
}

fn reconcile_listeners(
    id: NodeId,
    mounted: &mut BTreeMap<EventType, Listener>,
    listeners: Vec<Listener>,
    mutations: &mut Vec<Mutation>,
) {
    let next = listeners
        .into_iter()
        .map(|listener| (listener.event, listener))
        .collect::<BTreeMap<_, _>>();
    for (event, listener) in mounted.iter() {
        if next.get(event) != Some(listener) {
            mutations.push(Mutation::Unlisten {
                id,
                event: *event,
                handler: listener.handler,
            });
        }
    }
    for (event, listener) in &next {
        if mounted.get(event) != Some(listener) {
            mutations.push(Mutation::Listen {
                id,
                listener: *listener,
            });
        }
    }
    *mounted = next;
}

fn emit_order_mutations(
    parent: NodeId,
    mut current: Vec<NodeId>,
    desired: &[NodeId],
    mutations: &mut Vec<Mutation>,
) {
    for (index, child) in desired.iter().copied().enumerate() {
        if current.get(index) == Some(&child) {
            continue;
        }
        if let Some(position) = current.iter().position(|id| *id == child) {
            current.remove(position);
        }
        let before = current.get(index).copied();
        mutations.push(Mutation::InsertBefore {
            parent,
            child,
            before,
        });
        current.insert(index, child);
    }
}

fn ensure_unique_keys(views: &[View]) -> Result<(), LocalError> {
    let mut keys = BTreeSet::new();
    for view in views {
        if let Some(key) = &view.key {
            if !keys.insert(key.clone()) {
                return Err(LocalError::DuplicateKey(key.clone()));
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::alloc::{GlobalAlloc, Layout};
    use core::cell::Cell;
    use std::alloc::System;
    use vo_ui_core::{Primitive, Value};
    use vo_ui_plan::{
        ComponentPlan, LocalNodeId, PlanLimits, SlotKind, SlotValue, TemplateNode, UpdateSite,
    };
    use vo_ui_protocol::{ApplyError, ProtocolLimits, TreeMirror};

    struct TrackingAllocator;

    std::thread_local! {
        static TRACK_ALLOCATIONS: Cell<bool> = const { Cell::new(false) };
        static ALLOCATION_COUNT: Cell<usize> = const { Cell::new(0) };
    }

    unsafe impl GlobalAlloc for TrackingAllocator {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            TRACK_ALLOCATIONS.with(|tracking| {
                if tracking.get() {
                    ALLOCATION_COUNT.with(|count| count.set(count.get().saturating_add(1)));
                }
            });
            unsafe { System.alloc(layout) }
        }

        unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
            TRACK_ALLOCATIONS.with(|tracking| {
                if tracking.get() {
                    ALLOCATION_COUNT.with(|count| count.set(count.get().saturating_add(1)));
                }
            });
            unsafe { System.alloc_zeroed(layout) }
        }

        unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
            unsafe { System.dealloc(pointer, layout) }
        }

        unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
            TRACK_ALLOCATIONS.with(|tracking| {
                if tracking.get() {
                    ALLOCATION_COUNT.with(|count| count.set(count.get().saturating_add(1)));
                }
            });
            unsafe { System.realloc(pointer, layout, size) }
        }
    }

    #[global_allocator]
    static GLOBAL_ALLOCATOR: TrackingAllocator = TrackingAllocator;

    fn allocations_during<T>(operation: impl FnOnce() -> T) -> (T, usize) {
        ALLOCATION_COUNT.with(|count| count.set(0));
        TRACK_ALLOCATIONS.with(|tracking| tracking.set(true));
        let value = operation();
        TRACK_ALLOCATIONS.with(|tracking| tracking.set(false));
        let allocations = ALLOCATION_COUNT.with(Cell::get);
        (value, allocations)
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    enum TestRenderError {
        Rejected,
        Protocol,
    }

    struct TestRenderer {
        tree: TreeMirror,
        reject_next: bool,
    }

    #[derive(Default)]
    struct AllocationRenderer {
        revision: u64,
    }

    impl Renderer for AllocationRenderer {
        type Error = core::convert::Infallible;

        fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
            self.revision = batch.revision;
            Ok(())
        }
    }

    impl Renderer for TestRenderer {
        type Error = TestRenderError;

        fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
            if self.reject_next {
                self.reject_next = false;
                return Err(TestRenderError::Rejected);
            }
            self.tree
                .apply(batch)
                .map_err(|_error: ApplyError| TestRenderError::Protocol)
        }
    }

    fn runtime() -> UiRuntime<TestRenderer> {
        let root = NodeId::new(0, 1);
        UiRuntime::new(
            TestRenderer {
                tree: TreeMirror::new(1, root, ProtocolLimits::default()),
                reject_next: false,
            },
            1,
            root,
        )
    }

    fn template_runtime() -> TemplateRuntime<TestRenderer> {
        let root = NodeId::new(0, 1);
        TemplateRuntime::new(
            TestRenderer {
                tree: TreeMirror::new(1, root, ProtocolLimits::default()),
                reject_next: false,
            },
            1,
            root,
        )
    }

    fn direct_plan() -> ValidatedPlan {
        let mut plan = ComponentPlan::new(LocalNodeId::new(0));
        plan.slots = alloc::vec![SlotKind::Text, SlotKind::Property];
        plan.nodes = alloc::vec![
            TemplateNode::element(LocalNodeId::new(0), Primitive::Column)
                .child(LocalNodeId::new(1)),
            TemplateNode::text(LocalNodeId::new(1), ""),
        ];
        plan.updates = alloc::vec![
            UpdateSite::text(SlotId::new(0), LocalNodeId::new(1)),
            UpdateSite::property(SlotId::new(1), LocalNodeId::new(0), PropertyId::BACKGROUND,),
        ];
        plan.validate(PlanLimits::default()).unwrap()
    }

    #[test]
    fn root_fallback_projection_extracts_compiled_slots() {
        let plan = direct_plan();
        let view = View::element(Primitive::Column)
            .property(PropertyId::BACKGROUND, Value::Color(0x1122_33ff))
            .child(View::text("ready"));
        assert_eq!(
            project_template_slots(&plan, &view).unwrap(),
            alloc::vec![
                SlotValue::Text("ready".into()),
                SlotValue::Property(Value::Color(0x1122_33ff)),
            ]
        );

        let mismatched = View::element(Primitive::Row)
            .property(PropertyId::BACKGROUND, Value::Color(0x1122_33ff))
            .child(View::text("ready"));
        assert_eq!(
            project_template_slots(&plan, &mismatched),
            Err(TemplateProjectionError::NodeKindMismatch(LocalNodeId::new(
                0
            )))
        );
    }

    #[test]
    fn renderer_failure_keeps_runtime_revision_and_mounted_tree() {
        let mut runtime = runtime();
        runtime.mount(View::text("first")).unwrap();
        let original = runtime.renderer().tree.node(NodeId::new(1, 1)).unwrap();
        runtime.renderer_mut().reject_next = true;

        assert!(matches!(
            runtime.update(View::text("second")),
            Err(ReconcileError::Renderer(TestRenderError::Rejected))
        ));
        assert_eq!(runtime.revision(), 1);
        assert_eq!(runtime.renderer().tree.revision(), 1);
        assert_eq!(
            runtime.renderer().tree.node(original.id).unwrap().text,
            "first"
        );

        runtime.update(View::text("second")).unwrap();
        assert_eq!(
            runtime.renderer().tree.node(original.id).unwrap().text,
            "second"
        );
    }

    #[test]
    fn duplicate_sibling_keys_fail_before_renderer_commit() {
        let mut runtime = runtime();
        let view = View::fragment([
            View::text("first").key("same"),
            View::text("second").key("same"),
        ]);
        assert!(matches!(
            runtime.mount(view),
            Err(ReconcileError::DuplicateKey(Key::Text(key))) if key == "same"
        ));
        assert_eq!(runtime.revision(), 0);
        assert_eq!(runtime.renderer().tree.revision(), 0);
    }

    #[test]
    fn incompatible_keyed_replacement_reuses_slot_with_new_generation() {
        let mut runtime = runtime();
        runtime.mount(View::text("first").key("item")).unwrap();
        let first = runtime.renderer().tree.node(NodeId::new(1, 1)).unwrap().id;
        runtime
            .update(View::element(Primitive::Box).key("item"))
            .unwrap();
        let replacement = runtime.renderer().tree.node(NodeId::new(1, 2)).unwrap().id;
        assert_eq!(first.index(), replacement.index());
        assert_ne!(first.generation(), replacement.generation());
    }

    #[test]
    fn compiled_template_updates_only_precomputed_sites() {
        let mut runtime = template_runtime();
        runtime
            .mount(
                direct_plan(),
                alloc::vec![
                    SlotValue::Text("first".into()),
                    SlotValue::Property(Value::Color(0xff00_00ff)),
                ],
            )
            .unwrap();
        let root_child = runtime
            .renderer()
            .tree
            .node(NodeId::new(0, 1))
            .unwrap()
            .children[0];
        let text = runtime.renderer().tree.node(root_child).unwrap().children[0];
        assert_eq!(runtime.renderer().tree.node(text).unwrap().text, "first");

        let batch = runtime
            .update_slot(SlotId::new(0), SlotValue::Text("second".into()))
            .unwrap()
            .unwrap();
        assert_eq!(batch.mutations.len(), 1);
        assert!(matches!(
            &batch.mutations[0],
            Mutation::SetText { id, text: value } if *id == text && value == "second"
        ));
        assert_eq!(runtime.renderer().tree.node(text).unwrap().text, "second");
        assert!(runtime
            .update_slot(SlotId::new(0), SlotValue::Text("second".into()))
            .unwrap()
            .is_none());
    }

    #[test]
    fn warmed_scalar_slot_update_performs_zero_host_allocations() {
        let mut plan = ComponentPlan::new(LocalNodeId::new(0));
        plan.slots.push(SlotKind::Property);
        plan.nodes
            .push(TemplateNode::element(LocalNodeId::new(0), Primitive::Box));
        plan.updates.push(UpdateSite::property(
            SlotId::new(0),
            LocalNodeId::new(0),
            PropertyId::CHECKED,
        ));
        let plan = plan.validate(PlanLimits::default()).unwrap();
        let root = NodeId::new(0, 1);
        let mut runtime = TemplateRuntime::new(AllocationRenderer::default(), 1, root);
        runtime
            .mount(plan, alloc::vec![SlotValue::Property(Value::Bool(false))])
            .unwrap();
        runtime
            .update_slots_in_place(core::iter::once((
                SlotId::new(0),
                SlotValue::Property(Value::Bool(true)),
            )))
            .unwrap();
        runtime
            .update_slots_in_place(core::iter::once((
                SlotId::new(0),
                SlotValue::Property(Value::Bool(false)),
            )))
            .unwrap();

        let (mutation_count, allocations) = allocations_during(|| {
            runtime
                .update_slots_in_place(core::iter::once((
                    SlotId::new(0),
                    SlotValue::Property(Value::Bool(true)),
                )))
                .unwrap()
                .unwrap()
                .mutations
                .len()
        });
        assert_eq!(mutation_count, 1);
        assert_eq!(allocations, 0);
    }

    #[test]
    fn compiled_template_renderer_failure_rolls_back_slot_state() {
        let mut runtime = template_runtime();
        runtime
            .mount(
                direct_plan(),
                alloc::vec![
                    SlotValue::Text("first".into()),
                    SlotValue::Property(Value::Color(0)),
                ],
            )
            .unwrap();
        runtime.renderer_mut().reject_next = true;
        assert!(matches!(
            runtime.update_slot(SlotId::new(0), SlotValue::Text("second".into())),
            Err(TemplateError::Renderer(TestRenderError::Rejected))
        ));
        assert_eq!(runtime.revision(), 1);

        let retry = runtime
            .update_slot(SlotId::new(0), SlotValue::Text("second".into()))
            .unwrap();
        assert!(retry.is_some());
        assert_eq!(runtime.revision(), 2);
    }

    #[test]
    fn compiled_template_unmount_is_atomic_and_reuses_generations() {
        let mut runtime = template_runtime();
        let slots = || {
            alloc::vec![
                SlotValue::Text("value".into()),
                SlotValue::Property(Value::Color(0)),
            ]
        };
        runtime.mount(direct_plan(), slots()).unwrap();
        let first = runtime
            .renderer()
            .tree
            .node(NodeId::new(0, 1))
            .unwrap()
            .children[0];
        runtime.unmount().unwrap();
        assert!(runtime
            .renderer()
            .tree
            .node(NodeId::new(0, 1))
            .unwrap()
            .children
            .is_empty());

        runtime.mount(direct_plan(), slots()).unwrap();
        let second = runtime
            .renderer()
            .tree
            .node(NodeId::new(0, 1))
            .unwrap()
            .children[0];
        assert_eq!(first.index(), second.index());
        assert_ne!(first.generation(), second.generation());
    }

    #[test]
    fn compiled_template_reload_replaces_one_atomic_revision() {
        let mut runtime = template_runtime();
        let slots = |text: &str| {
            alloc::vec![
                SlotValue::Text(text.into()),
                SlotValue::Property(Value::Color(0)),
            ]
        };
        runtime.mount(direct_plan(), slots("old")).unwrap();
        let host = NodeId::new(0, 1);
        let previous_root = runtime.renderer().tree.node(host).unwrap().children[0];

        runtime.renderer_mut().reject_next = true;
        assert!(matches!(
            runtime.replace(direct_plan(), slots("new")),
            Err(TemplateError::Renderer(TestRenderError::Rejected))
        ));
        assert_eq!(runtime.revision(), 1);
        assert_eq!(
            runtime.renderer().tree.node(host).unwrap().children,
            alloc::vec![previous_root]
        );

        let batch = runtime.replace(direct_plan(), slots("new")).unwrap();
        assert_eq!(batch.revision, 2);
        let next_root = runtime.renderer().tree.node(host).unwrap().children[0];
        let next_text = runtime.renderer().tree.node(next_root).unwrap().children[0];
        assert_ne!(previous_root, next_root);
        assert!(runtime.renderer().tree.node(previous_root).is_none());
        assert_eq!(runtime.renderer().tree.node(next_text).unwrap().text, "new");
    }
}
