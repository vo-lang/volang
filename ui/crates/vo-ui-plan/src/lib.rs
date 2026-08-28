#![no_std]

extern crate alloc;

use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use core::ops::Range;
use vo_ui_core::{Listener, Primitive, Property, PropertyId, Value};

mod codec;

pub use codec::{decode_plan, encode_plan, PlanCodecError};

pub const COMPONENT_PLAN_ABI_VERSION: u16 = 1;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct LocalNodeId(u32);

impl LocalNodeId {
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn index(self) -> u32 {
        self.0
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SlotId(u32);

impl SlotId {
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn index(self) -> u32 {
        self.0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SlotKind {
    Text,
    Property,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SlotValue {
    Text(String),
    Property(Value),
}

impl SlotValue {
    pub const fn kind(&self) -> SlotKind {
        match self {
            Self::Text(_) => SlotKind::Text,
            Self::Property(_) => SlotKind::Property,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TemplateNodeKind {
    Element(Primitive),
    Text,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TemplateNode {
    pub id: LocalNodeId,
    pub kind: TemplateNodeKind,
    pub text: String,
    pub properties: Vec<Property>,
    pub listeners: Vec<Listener>,
    pub children: Vec<LocalNodeId>,
}

impl TemplateNode {
    pub fn element(id: LocalNodeId, primitive: Primitive) -> Self {
        Self {
            id,
            kind: TemplateNodeKind::Element(primitive),
            text: String::new(),
            properties: Vec::new(),
            listeners: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn text(id: LocalNodeId, text: impl Into<String>) -> Self {
        Self {
            id,
            kind: TemplateNodeKind::Text,
            text: text.into(),
            properties: Vec::new(),
            listeners: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn property(mut self, property: Property) -> Self {
        self.properties.push(property);
        self
    }

    pub fn listener(mut self, listener: Listener) -> Self {
        self.listeners.push(listener);
        self
    }

    pub fn child(mut self, child: LocalNodeId) -> Self {
        self.children.push(child);
        self
    }

    pub fn children(mut self, children: impl IntoIterator<Item = LocalNodeId>) -> Self {
        self.children.extend(children);
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum DirectMutation {
    SetText {
        target: LocalNodeId,
    },
    SetProperty {
        target: LocalNodeId,
        property: PropertyId,
    },
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct UpdateSite {
    pub slot: SlotId,
    pub mutation: DirectMutation,
}

impl UpdateSite {
    pub const fn text(slot: SlotId, target: LocalNodeId) -> Self {
        Self {
            slot,
            mutation: DirectMutation::SetText { target },
        }
    }

    pub const fn property(slot: SlotId, target: LocalNodeId, property: PropertyId) -> Self {
        Self {
            slot,
            mutation: DirectMutation::SetProperty { target, property },
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentPlan {
    pub abi_version: u16,
    pub root: LocalNodeId,
    pub slots: Vec<SlotKind>,
    pub nodes: Vec<TemplateNode>,
    /// Update sites are sorted by slot so execution is O(changed bindings).
    pub updates: Vec<UpdateSite>,
}

impl ComponentPlan {
    pub fn new(root: LocalNodeId) -> Self {
        Self {
            abi_version: COMPONENT_PLAN_ABI_VERSION,
            root,
            slots: Vec::new(),
            nodes: Vec::new(),
            updates: Vec::new(),
        }
    }

    pub fn validate(self, limits: PlanLimits) -> Result<ValidatedPlan, PlanError> {
        validate_plan(&self, limits)?;
        let mut slot_ranges = alloc::vec![0..0; self.slots.len()];
        let mut cursor = 0;
        while cursor < self.updates.len() {
            let slot = self.updates[cursor].slot.index() as usize;
            let start = cursor;
            while cursor < self.updates.len() && self.updates[cursor].slot.index() as usize == slot
            {
                cursor += 1;
            }
            slot_ranges[slot] = start..cursor;
        }
        Ok(ValidatedPlan {
            plan: self,
            slot_ranges,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValidatedPlan {
    plan: ComponentPlan,
    slot_ranges: Vec<Range<usize>>,
}

impl ValidatedPlan {
    pub const fn root(&self) -> LocalNodeId {
        self.plan.root
    }

    pub fn nodes(&self) -> &[TemplateNode] {
        &self.plan.nodes
    }

    pub fn node(&self, id: LocalNodeId) -> &TemplateNode {
        &self.plan.nodes[id.index() as usize]
    }

    pub fn slots(&self) -> &[SlotKind] {
        &self.plan.slots
    }

    pub fn slot_kind(&self, id: SlotId) -> Option<SlotKind> {
        self.plan.slots.get(id.index() as usize).copied()
    }

    pub fn update_sites(&self, id: SlotId) -> Option<&[UpdateSite]> {
        let range = self.slot_ranges.get(id.index() as usize)?.clone();
        Some(&self.plan.updates[range])
    }

    pub const fn as_plan(&self) -> &ComponentPlan {
        &self.plan
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PlanLimits {
    pub max_plan_bytes: usize,
    pub max_nodes: usize,
    pub max_slots: usize,
    pub max_updates: usize,
    pub max_children_per_node: usize,
    pub max_properties_per_node: usize,
    pub max_listeners_per_node: usize,
    pub max_static_value_bytes: usize,
}

impl Default for PlanLimits {
    fn default() -> Self {
        Self {
            max_plan_bytes: 16 * 1024 * 1024,
            max_nodes: 100_000,
            max_slots: 100_000,
            max_updates: 200_000,
            max_children_per_node: 50_000,
            max_properties_per_node: 512,
            max_listeners_per_node: 256,
            max_static_value_bytes: 4 * 1024 * 1024,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PlanError {
    UnsupportedAbi {
        found: u16,
    },
    NodeLimitExceeded,
    SlotLimitExceeded,
    UpdateLimitExceeded,
    NodeIdentityNotDense {
        position: usize,
        found: LocalNodeId,
    },
    InvalidRoot(LocalNodeId),
    InvalidChild {
        parent: LocalNodeId,
        child: LocalNodeId,
    },
    DuplicateChild {
        parent: LocalNodeId,
        child: LocalNodeId,
    },
    MultipleParents(LocalNodeId),
    RootHasParent,
    UnreachableNode(LocalNodeId),
    TextHasChildren(LocalNodeId),
    TextHasProperties(LocalNodeId),
    TextHasListeners(LocalNodeId),
    ChildLimitExceeded(LocalNodeId),
    PropertyLimitExceeded(LocalNodeId),
    ListenerLimitExceeded(LocalNodeId),
    StaticValueLimitExceeded(LocalNodeId),
    DuplicateProperty {
        node: LocalNodeId,
        property: PropertyId,
    },
    DuplicateListener(LocalNodeId),
    UpdatesNotSorted,
    InvalidSlot(SlotId),
    InvalidUpdateTarget(LocalNodeId),
    SlotKindMismatch(SlotId),
    TextTargetRequired(LocalNodeId),
    ElementTargetRequired(LocalNodeId),
    DuplicateUpdateTarget,
    StaticDynamicPropertyConflict {
        node: LocalNodeId,
        property: PropertyId,
    },
}

impl fmt::Display for PlanError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "invalid UI component plan: {self:?}")
    }
}

fn validate_plan(plan: &ComponentPlan, limits: PlanLimits) -> Result<(), PlanError> {
    if plan.abi_version != COMPONENT_PLAN_ABI_VERSION {
        return Err(PlanError::UnsupportedAbi {
            found: plan.abi_version,
        });
    }
    if plan.nodes.len() > limits.max_nodes {
        return Err(PlanError::NodeLimitExceeded);
    }
    if plan.slots.len() > limits.max_slots {
        return Err(PlanError::SlotLimitExceeded);
    }
    if plan.updates.len() > limits.max_updates {
        return Err(PlanError::UpdateLimitExceeded);
    }
    if plan.root.index() as usize >= plan.nodes.len() {
        return Err(PlanError::InvalidRoot(plan.root));
    }

    let mut parents = alloc::vec![0_u32; plan.nodes.len()];
    for (position, node) in plan.nodes.iter().enumerate() {
        if node.id.index() as usize != position {
            return Err(PlanError::NodeIdentityNotDense {
                position,
                found: node.id,
            });
        }
        validate_node(node, limits)?;
        let mut children = BTreeSet::new();
        for child in &node.children {
            if child.index() as usize >= plan.nodes.len() {
                return Err(PlanError::InvalidChild {
                    parent: node.id,
                    child: *child,
                });
            }
            if !children.insert(*child) {
                return Err(PlanError::DuplicateChild {
                    parent: node.id,
                    child: *child,
                });
            }
            parents[child.index() as usize] += 1;
            if parents[child.index() as usize] > 1 {
                return Err(PlanError::MultipleParents(*child));
            }
        }
    }
    if parents[plan.root.index() as usize] != 0 {
        return Err(PlanError::RootHasParent);
    }

    let mut reached = alloc::vec![false; plan.nodes.len()];
    let mut pending = alloc::vec![plan.root];
    while let Some(node) = pending.pop() {
        if reached[node.index() as usize] {
            continue;
        }
        reached[node.index() as usize] = true;
        pending.extend(plan.nodes[node.index() as usize].children.iter().copied());
    }
    if let Some(index) = reached.iter().position(|reached| !reached) {
        return Err(PlanError::UnreachableNode(LocalNodeId::new(index as u32)));
    }

    let mut previous_slot = None;
    let mut targets = BTreeSet::new();
    for update in &plan.updates {
        if previous_slot.is_some_and(|previous| update.slot < previous) {
            return Err(PlanError::UpdatesNotSorted);
        }
        previous_slot = Some(update.slot);
        let Some(slot_kind) = plan.slots.get(update.slot.index() as usize).copied() else {
            return Err(PlanError::InvalidSlot(update.slot));
        };
        if !targets.insert(update.mutation) {
            return Err(PlanError::DuplicateUpdateTarget);
        }
        validate_update(plan, *update, slot_kind)?;
    }
    Ok(())
}

fn validate_node(node: &TemplateNode, limits: PlanLimits) -> Result<(), PlanError> {
    if node.children.len() > limits.max_children_per_node {
        return Err(PlanError::ChildLimitExceeded(node.id));
    }
    if node.properties.len() > limits.max_properties_per_node {
        return Err(PlanError::PropertyLimitExceeded(node.id));
    }
    if node.listeners.len() > limits.max_listeners_per_node {
        return Err(PlanError::ListenerLimitExceeded(node.id));
    }
    if node.text.len() > limits.max_static_value_bytes
        || node
            .properties
            .iter()
            .any(|property| value_bytes(&property.value) > limits.max_static_value_bytes)
    {
        return Err(PlanError::StaticValueLimitExceeded(node.id));
    }
    match node.kind {
        TemplateNodeKind::Text => {
            if !node.children.is_empty() {
                return Err(PlanError::TextHasChildren(node.id));
            }
            if !node.properties.is_empty() {
                return Err(PlanError::TextHasProperties(node.id));
            }
            if !node.listeners.is_empty() {
                return Err(PlanError::TextHasListeners(node.id));
            }
        }
        TemplateNodeKind::Element(_) => {
            let mut properties = BTreeSet::new();
            for property in &node.properties {
                if !properties.insert(property.id) {
                    return Err(PlanError::DuplicateProperty {
                        node: node.id,
                        property: property.id,
                    });
                }
            }
            let mut events = BTreeSet::new();
            for listener in &node.listeners {
                if !events.insert(listener.event) {
                    return Err(PlanError::DuplicateListener(node.id));
                }
            }
        }
    }
    Ok(())
}

fn validate_update(
    plan: &ComponentPlan,
    update: UpdateSite,
    slot_kind: SlotKind,
) -> Result<(), PlanError> {
    let target = match update.mutation {
        DirectMutation::SetText { target } | DirectMutation::SetProperty { target, .. } => target,
    };
    let Some(node) = plan.nodes.get(target.index() as usize) else {
        return Err(PlanError::InvalidUpdateTarget(target));
    };
    match update.mutation {
        DirectMutation::SetText { .. } => {
            if slot_kind != SlotKind::Text {
                return Err(PlanError::SlotKindMismatch(update.slot));
            }
            if node.kind != TemplateNodeKind::Text {
                return Err(PlanError::TextTargetRequired(target));
            }
        }
        DirectMutation::SetProperty { property, .. } => {
            if slot_kind != SlotKind::Property {
                return Err(PlanError::SlotKindMismatch(update.slot));
            }
            if !matches!(node.kind, TemplateNodeKind::Element(_)) {
                return Err(PlanError::ElementTargetRequired(target));
            }
            if node.properties.iter().any(|item| item.id == property) {
                return Err(PlanError::StaticDynamicPropertyConflict {
                    node: target,
                    property,
                });
            }
        }
    }
    Ok(())
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
    use alloc::string::ToString;
    use vo_ui_core::{EventType, HandlerId};

    fn valid_plan() -> ComponentPlan {
        ComponentPlan {
            abi_version: COMPONENT_PLAN_ABI_VERSION,
            root: LocalNodeId::new(0),
            slots: alloc::vec![SlotKind::Text, SlotKind::Property],
            nodes: alloc::vec![
                TemplateNode::element(LocalNodeId::new(0), Primitive::Column)
                    .property(Property::new(PropertyId::GAP, 8_i64))
                    .listener(Listener::new(EventType::CLICK, HandlerId::new(0, 1)))
                    .child(LocalNodeId::new(1)),
                TemplateNode::text(LocalNodeId::new(1), ""),
            ],
            updates: alloc::vec![
                UpdateSite::text(SlotId::new(0), LocalNodeId::new(1)),
                UpdateSite::property(SlotId::new(1), LocalNodeId::new(0), PropertyId::BACKGROUND,),
            ],
        }
    }

    #[test]
    fn validates_dense_rooted_plan_and_indexes_updates_by_slot() {
        let plan = valid_plan().validate(PlanLimits::default()).unwrap();
        assert_eq!(plan.nodes().len(), 2);
        assert_eq!(plan.slot_kind(SlotId::new(0)), Some(SlotKind::Text));
        assert_eq!(plan.update_sites(SlotId::new(0)).unwrap().len(), 1);
        assert!(plan.update_sites(SlotId::new(9)).is_none());
    }

    #[test]
    fn rejects_disconnected_cycles_without_recursive_validation() {
        let mut plan = valid_plan();
        plan.nodes.push(
            TemplateNode::element(LocalNodeId::new(2), Primitive::Box).child(LocalNodeId::new(3)),
        );
        plan.nodes.push(
            TemplateNode::element(LocalNodeId::new(3), Primitive::Box).child(LocalNodeId::new(2)),
        );
        assert_eq!(
            plan.validate(PlanLimits::default()).unwrap_err(),
            PlanError::UnreachableNode(LocalNodeId::new(2))
        );
    }

    #[test]
    fn rejects_unsorted_and_conflicting_update_sites() {
        let mut plan = valid_plan();
        plan.updates.swap(0, 1);
        assert_eq!(
            plan.validate(PlanLimits::default()).unwrap_err(),
            PlanError::UpdatesNotSorted
        );

        let mut plan = valid_plan();
        plan.nodes[0]
            .properties
            .push(Property::new(PropertyId::BACKGROUND, 0_i64));
        assert_eq!(
            plan.validate(PlanLimits::default()).unwrap_err(),
            PlanError::StaticDynamicPropertyConflict {
                node: LocalNodeId::new(0),
                property: PropertyId::BACKGROUND,
            }
        );
    }

    #[test]
    fn text_slot_values_keep_text_type_at_the_plan_boundary() {
        let value = SlotValue::Text("ready".to_string());
        assert_eq!(value.kind(), SlotKind::Text);
    }
}
