#![no_std]

extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{
    EventType, HandlerId, Listener, NodeId, Primitive, Property, PropertyId, UiEvent, Value,
};

mod codec;

pub use codec::{decode_batch, decode_event, encode_batch, encode_event, CodecError};

/// Renderer-to-runtime event envelope. The session epoch rejects events left
/// behind by reloads, remounts, or a replaced platform surface.
#[derive(Clone, Debug, PartialEq)]
pub struct EventEnvelope {
    pub session_epoch: u64,
    pub event: UiEvent,
}

impl EventEnvelope {
    pub const fn new(session_epoch: u64, event: UiEvent) -> Self {
        Self {
            session_epoch,
            event,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NodeKind {
    Element(Primitive),
    Text,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Mutation {
    Create {
        id: NodeId,
        kind: NodeKind,
    },
    SetText {
        id: NodeId,
        text: String,
    },
    SetProperty {
        id: NodeId,
        property: Property,
    },
    RemoveProperty {
        id: NodeId,
        property: PropertyId,
    },
    Listen {
        id: NodeId,
        listener: Listener,
    },
    Unlisten {
        id: NodeId,
        event: EventType,
        handler: HandlerId,
    },
    InsertBefore {
        parent: NodeId,
        child: NodeId,
        before: Option<NodeId>,
    },
    Remove {
        parent: NodeId,
        child: NodeId,
    },
    Delete {
        id: NodeId,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct MutationBatch {
    pub session_epoch: u64,
    pub revision: u64,
    pub mutations: Vec<Mutation>,
}

impl MutationBatch {
    pub fn new(session_epoch: u64, revision: u64, mutations: Vec<Mutation>) -> Self {
        Self {
            session_epoch,
            revision,
            mutations,
        }
    }
}

pub trait Renderer {
    type Error;

    /// Applies one complete revision. Implementations must leave their previous
    /// tree intact when this call returns an error.
    fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error>;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProtocolLimits {
    pub max_batch_bytes: usize,
    pub max_event_bytes: usize,
    pub max_nodes: usize,
    pub max_mutations_per_batch: usize,
    pub max_children_per_node: usize,
    pub max_properties_per_node: usize,
    pub max_text_bytes: usize,
    pub max_value_bytes: usize,
}

impl Default for ProtocolLimits {
    fn default() -> Self {
        Self {
            max_batch_bytes: 16 * 1024 * 1024,
            max_event_bytes: 4 * 1024 * 1024,
            max_nodes: 100_000,
            max_mutations_per_batch: 100_000,
            max_children_per_node: 50_000,
            max_properties_per_node: 512,
            max_text_bytes: 4 * 1024 * 1024,
            max_value_bytes: 4 * 1024 * 1024,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ApplyError {
    SessionEpochMismatch,
    RevisionMismatch,
    MutationLimitExceeded,
    NodeLimitExceeded,
    NodeAlreadyExists(NodeId),
    MissingNode(NodeId),
    RootMutationDenied,
    ParentCannotContainChildren(NodeId),
    ChildAlreadyHasDifferentParent(NodeId),
    ChildRelationshipMismatch,
    BeforeSiblingMismatch,
    CycleDetected,
    NodeStillAttached(NodeId),
    NodeStillHasChildren(NodeId),
    TextTargetRequired(NodeId),
    ElementTargetRequired(NodeId),
    ChildLimitExceeded(NodeId),
    PropertyLimitExceeded(NodeId),
    TextLimitExceeded,
    ValueLimitExceeded,
    ListenerMismatch,
}

impl fmt::Display for ApplyError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{self:?}")
    }
}

#[derive(Clone, Debug, PartialEq)]
struct NodeRecord {
    kind: NodeKind,
    text: String,
    properties: BTreeMap<PropertyId, Value>,
    listeners: BTreeMap<EventType, Listener>,
    parent: Option<NodeId>,
    children: Vec<NodeId>,
}

impl NodeRecord {
    fn new(kind: NodeKind) -> Self {
        Self {
            kind,
            text: String::new(),
            properties: BTreeMap::new(),
            listeners: BTreeMap::new(),
            parent: None,
            children: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NodeSnapshot {
    pub id: NodeId,
    pub kind: NodeKind,
    pub text: String,
    pub properties: BTreeMap<PropertyId, Value>,
    pub listeners: BTreeMap<EventType, Listener>,
    pub parent: Option<NodeId>,
    pub children: Vec<NodeId>,
}

#[derive(Clone, Debug)]
pub struct TreeMirror {
    session_epoch: u64,
    revision: u64,
    root: NodeId,
    limits: ProtocolLimits,
    nodes: BTreeMap<NodeId, NodeRecord>,
}

impl TreeMirror {
    pub fn new(session_epoch: u64, root: NodeId, limits: ProtocolLimits) -> Self {
        let mut nodes = BTreeMap::new();
        nodes.insert(root, NodeRecord::new(NodeKind::Element(Primitive::Root)));
        Self {
            session_epoch,
            revision: 0,
            root,
            limits,
            nodes,
        }
    }

    pub const fn session_epoch(&self) -> u64 {
        self.session_epoch
    }

    pub const fn revision(&self) -> u64 {
        self.revision
    }

    pub const fn root(&self) -> NodeId {
        self.root
    }

    pub fn node(&self, id: NodeId) -> Option<NodeSnapshot> {
        self.nodes.get(&id).map(|node| NodeSnapshot {
            id,
            kind: node.kind,
            text: node.text.clone(),
            properties: node.properties.clone(),
            listeners: node.listeners.clone(),
            parent: node.parent,
            children: node.children.clone(),
        })
    }

    pub fn nodes(&self) -> impl Iterator<Item = NodeSnapshot> + '_ {
        self.nodes.keys().filter_map(|id| self.node(*id))
    }

    pub fn apply(&mut self, batch: &MutationBatch) -> Result<(), ApplyError> {
        if batch.session_epoch != self.session_epoch {
            return Err(ApplyError::SessionEpochMismatch);
        }
        if batch.revision != self.revision.saturating_add(1) {
            return Err(ApplyError::RevisionMismatch);
        }
        if batch.mutations.len() > self.limits.max_mutations_per_batch {
            return Err(ApplyError::MutationLimitExceeded);
        }
        let mut staged = self.clone();
        for mutation in &batch.mutations {
            staged.apply_one(mutation)?;
        }
        staged.revision = batch.revision;
        *self = staged;
        Ok(())
    }

    fn apply_one(&mut self, mutation: &Mutation) -> Result<(), ApplyError> {
        match mutation {
            Mutation::Create { id, kind } => {
                if *id == self.root {
                    return Err(ApplyError::RootMutationDenied);
                }
                if self.nodes.contains_key(id) {
                    return Err(ApplyError::NodeAlreadyExists(*id));
                }
                if self.nodes.len() >= self.limits.max_nodes {
                    return Err(ApplyError::NodeLimitExceeded);
                }
                self.nodes.insert(*id, NodeRecord::new(*kind));
            }
            Mutation::SetText { id, text } => {
                if text.len() > self.limits.max_text_bytes {
                    return Err(ApplyError::TextLimitExceeded);
                }
                let node = self.node_mut(*id)?;
                if node.kind != NodeKind::Text {
                    return Err(ApplyError::TextTargetRequired(*id));
                }
                node.text.clone_from(text);
            }
            Mutation::SetProperty { id, property } => {
                if value_bytes(&property.value) > self.limits.max_value_bytes {
                    return Err(ApplyError::ValueLimitExceeded);
                }
                let limit = self.limits.max_properties_per_node;
                let node = self.node_mut(*id)?;
                if !matches!(node.kind, NodeKind::Element(_)) {
                    return Err(ApplyError::ElementTargetRequired(*id));
                }
                if !node.properties.contains_key(&property.id) && node.properties.len() >= limit {
                    return Err(ApplyError::PropertyLimitExceeded(*id));
                }
                node.properties.insert(property.id, property.value.clone());
            }
            Mutation::RemoveProperty { id, property } => {
                let node = self.node_mut(*id)?;
                if !matches!(node.kind, NodeKind::Element(_)) {
                    return Err(ApplyError::ElementTargetRequired(*id));
                }
                node.properties.remove(property);
            }
            Mutation::Listen { id, listener } => {
                let node = self.node_mut(*id)?;
                if !matches!(node.kind, NodeKind::Element(_)) {
                    return Err(ApplyError::ElementTargetRequired(*id));
                }
                node.listeners.insert(listener.event, *listener);
            }
            Mutation::Unlisten { id, event, handler } => {
                let node = self.node_mut(*id)?;
                if node.listeners.get(event).map(|item| item.handler) != Some(*handler) {
                    return Err(ApplyError::ListenerMismatch);
                }
                node.listeners.remove(event);
            }
            Mutation::InsertBefore {
                parent,
                child,
                before,
            } => self.insert_before(*parent, *child, *before)?,
            Mutation::Remove { parent, child } => self.remove(*parent, *child)?,
            Mutation::Delete { id } => self.delete(*id)?,
        }
        Ok(())
    }

    fn insert_before(
        &mut self,
        parent: NodeId,
        child: NodeId,
        before: Option<NodeId>,
    ) -> Result<(), ApplyError> {
        if parent == child || self.is_ancestor(child, parent)? {
            return Err(ApplyError::CycleDetected);
        }
        let child_parent = self.node_ref(child)?.parent;
        if child_parent.is_some() && child_parent != Some(parent) {
            return Err(ApplyError::ChildAlreadyHasDifferentParent(child));
        }
        let limit = self.limits.max_children_per_node;
        let parent_node = self.node_ref(parent)?;
        if !matches!(parent_node.kind, NodeKind::Element(_)) {
            return Err(ApplyError::ParentCannotContainChildren(parent));
        }
        if let Some(before) = before {
            if before == child || !parent_node.children.contains(&before) {
                return Err(ApplyError::BeforeSiblingMismatch);
            }
        }
        let parent_node = self.node_mut(parent)?;
        if let Some(position) = parent_node.children.iter().position(|id| *id == child) {
            parent_node.children.remove(position);
        } else if parent_node.children.len() >= limit {
            return Err(ApplyError::ChildLimitExceeded(parent));
        }
        let position = match before {
            Some(before) => parent_node
                .children
                .iter()
                .position(|id| *id == before)
                .ok_or(ApplyError::BeforeSiblingMismatch)?,
            None => parent_node.children.len(),
        };
        parent_node.children.insert(position, child);
        self.node_mut(child)?.parent = Some(parent);
        Ok(())
    }

    fn remove(&mut self, parent: NodeId, child: NodeId) -> Result<(), ApplyError> {
        if self.node_ref(child)?.parent != Some(parent) {
            return Err(ApplyError::ChildRelationshipMismatch);
        }
        let parent_node = self.node_mut(parent)?;
        let position = parent_node
            .children
            .iter()
            .position(|id| *id == child)
            .ok_or(ApplyError::ChildRelationshipMismatch)?;
        parent_node.children.remove(position);
        self.node_mut(child)?.parent = None;
        Ok(())
    }

    fn delete(&mut self, id: NodeId) -> Result<(), ApplyError> {
        if id == self.root {
            return Err(ApplyError::RootMutationDenied);
        }
        let node = self.node_ref(id)?;
        if node.parent.is_some() {
            return Err(ApplyError::NodeStillAttached(id));
        }
        if !node.children.is_empty() {
            return Err(ApplyError::NodeStillHasChildren(id));
        }
        self.nodes.remove(&id);
        Ok(())
    }

    fn is_ancestor(&self, ancestor: NodeId, mut node: NodeId) -> Result<bool, ApplyError> {
        loop {
            let Some(parent) = self.node_ref(node)?.parent else {
                return Ok(false);
            };
            if parent == ancestor {
                return Ok(true);
            }
            node = parent;
        }
    }

    fn node_ref(&self, id: NodeId) -> Result<&NodeRecord, ApplyError> {
        self.nodes.get(&id).ok_or(ApplyError::MissingNode(id))
    }

    fn node_mut(&mut self, id: NodeId) -> Result<&mut NodeRecord, ApplyError> {
        self.nodes.get_mut(&id).ok_or(ApplyError::MissingNode(id))
    }
}

fn value_bytes(value: &Value) -> usize {
    match value {
        Value::Text(value) => value.len(),
        Value::Bytes(value) => value.len(),
        Value::Bool(_) | Value::I64(_) | Value::F64(_) | Value::Color(_) | Value::Length(_) => 8,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn id(index: u32) -> NodeId {
        NodeId::new(index, 1)
    }

    #[test]
    fn failed_batch_is_atomic() {
        let root = id(0);
        let mut tree = TreeMirror::new(7, root, ProtocolLimits::default());
        let batch = MutationBatch::new(
            7,
            1,
            alloc::vec![
                Mutation::Create {
                    id: id(1),
                    kind: NodeKind::Text,
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: id(99),
                    before: None,
                },
            ],
        );

        assert_eq!(tree.apply(&batch), Err(ApplyError::MissingNode(id(99))));
        assert_eq!(tree.revision(), 0);
        assert!(tree.node(id(1)).is_none());
    }

    #[test]
    fn insert_before_moves_an_existing_child() {
        let root = id(0);
        let mut tree = TreeMirror::new(1, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            1,
            1,
            alloc::vec![
                Mutation::Create {
                    id: id(1),
                    kind: NodeKind::Text,
                },
                Mutation::Create {
                    id: id(2),
                    kind: NodeKind::Text,
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: id(1),
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: id(2),
                    before: None,
                },
            ],
        ))
        .unwrap();
        tree.apply(&MutationBatch::new(
            1,
            2,
            alloc::vec![Mutation::InsertBefore {
                parent: root,
                child: id(2),
                before: Some(id(1)),
            }],
        ))
        .unwrap();

        assert_eq!(tree.node(root).unwrap().children, alloc::vec![id(2), id(1)]);
    }
}
