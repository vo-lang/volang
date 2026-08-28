#![no_std]

extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{NodeId, Primitive, PropertyId, Value};
use vo_ui_layout::{LayoutSnapshot, Rect, Size};
use vo_ui_protocol::{NodeKind, NodeSnapshot, TreeMirror};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AccessibilityLimits {
    pub max_nodes: usize,
    pub max_depth: usize,
    pub max_text_bytes: usize,
}

impl Default for AccessibilityLimits {
    fn default() -> Self {
        Self {
            max_nodes: 100_000,
            max_depth: 1_024,
            max_text_bytes: 16 * 1024 * 1024,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AccessibilityRole {
    Root,
    Group,
    Presentation,
    StaticText,
    Paragraph,
    Button,
    TextBox,
    Switch,
    Slider,
    Image,
    Alert,
    Dialog,
    Heading,
    Status,
    ProgressIndicator,
    Separator,
    Link,
    Navigation,
    Toolbar,
    List,
    ListItem,
    RadioGroup,
    RadioButton,
    ComboBox,
    ListBox,
    Option,
    AlertDialog,
    Tooltip,
    MenuBar,
    Menu,
    MenuItem,
    MenuItemCheckBox,
    MenuItemRadio,
    TabList,
    Tab,
    TabPanel,
    Grid,
    Row,
    GridCell,
    ColumnHeader,
    RowHeader,
    Tree,
    TreeItem,
    Custom(String),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AccessibilityCurrent {
    False,
    True,
    Page,
    Step,
    Location,
    Date,
    Time,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct AccessibilityState {
    pub disabled: bool,
    pub required: bool,
    pub invalid: bool,
    pub modal: bool,
    pub checked: Option<bool>,
    pub selected: Option<bool>,
    pub expanded: Option<bool>,
    pub pressed: Option<bool>,
    pub current: Option<AccessibilityCurrent>,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct AccessibilityActions {
    pub focus: bool,
    pub invoke: bool,
    pub set_value: bool,
    pub toggle: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AccessibilityNode {
    pub id: NodeId,
    pub role: AccessibilityRole,
    pub name: String,
    pub description: String,
    pub value: String,
    pub bounds: Rect,
    pub state: AccessibilityState,
    pub actions: AccessibilityActions,
    pub children: Vec<NodeId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AccessibilityTree {
    pub revision: u64,
    pub viewport: Size,
    pub root: NodeId,
    nodes: BTreeMap<NodeId, AccessibilityNode>,
}

impl AccessibilityTree {
    pub fn get(&self, id: NodeId) -> Option<&AccessibilityNode> {
        self.nodes.get(&id)
    }

    pub fn iter(&self) -> impl Iterator<Item = &AccessibilityNode> {
        self.nodes.values()
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AccessibilityError {
    InvalidLimits,
    RevisionMismatch,
    MissingNode(NodeId),
    MissingLayout(NodeId),
    RootMustBeElement,
    InvalidProperty(NodeId, PropertyId),
    NodeLimitExceeded,
    TextLimitExceeded,
    DepthLimitExceeded,
}

impl fmt::Display for AccessibilityError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "UI accessibility tree generation failed: {self:?}"
        )
    }
}

struct Builder<'a> {
    tree: &'a TreeMirror,
    layout: &'a LayoutSnapshot,
    limits: AccessibilityLimits,
    nodes: BTreeMap<NodeId, AccessibilityNode>,
    visited_nodes: usize,
    text_bytes: usize,
}

pub fn build_accessibility_tree(
    tree: &TreeMirror,
    layout: &LayoutSnapshot,
    limits: AccessibilityLimits,
) -> Result<AccessibilityTree, AccessibilityError> {
    if limits.max_nodes == 0 || limits.max_depth == 0 || limits.max_text_bytes == 0 {
        return Err(AccessibilityError::InvalidLimits);
    }
    if tree.revision() != layout.revision {
        return Err(AccessibilityError::RevisionMismatch);
    }
    let root = tree.root();
    let mut builder = Builder {
        tree,
        layout,
        limits,
        nodes: BTreeMap::new(),
        visited_nodes: 0,
        text_bytes: 0,
    };
    if builder.visit_element(root, 0)?.is_none() {
        return Err(AccessibilityError::RootMustBeElement);
    }
    Ok(AccessibilityTree {
        revision: layout.revision,
        viewport: layout.viewport,
        root,
        nodes: builder.nodes,
    })
}

impl Builder<'_> {
    fn visit_element(
        &mut self,
        id: NodeId,
        depth: usize,
    ) -> Result<Option<NodeId>, AccessibilityError> {
        if depth >= self.limits.max_depth {
            return Err(AccessibilityError::DepthLimitExceeded);
        }
        let node = self
            .tree
            .node(id)
            .ok_or(AccessibilityError::MissingNode(id))?;
        let NodeKind::Element(primitive) = node.kind else {
            return Ok(None);
        };
        if bool_property(&node, id, PropertyId::HIDDEN)?.unwrap_or(false)
            || bool_property(&node, id, PropertyId::ACCESSIBILITY_HIDDEN)?.unwrap_or(false)
        {
            return Ok(None);
        }
        if self.visited_nodes >= self.limits.max_nodes {
            return Err(AccessibilityError::NodeLimitExceeded);
        }
        self.visited_nodes += 1;
        let layout = self
            .layout
            .get(id)
            .copied()
            .ok_or(AccessibilityError::MissingLayout(id))?;
        let role = role(&node, primitive)?;
        let mut children = Vec::new();
        for child in &node.children {
            if let Some(child) = self.visit_element(*child, depth + 1)? {
                children.push(child);
            }
        }
        let mut name = text_property(&node, id, PropertyId::ACCESSIBLE_NAME)?.unwrap_or_default();
        if name.is_empty() && role_uses_descendant_text(&role) {
            self.collect_descendant_text(id, depth, &mut name)?;
        }
        let description =
            text_property(&node, id, PropertyId::ACCESSIBLE_DESCRIPTION)?.unwrap_or_default();
        let value = accessible_value_property(&node, id)?;
        self.charge_text(name.len() + description.len() + value.len())?;
        let bounds = layout
            .clip
            .map_or(layout.rect, |clip| layout.rect.intersection(clip));
        let state = AccessibilityState {
            disabled: bool_property(&node, id, PropertyId::DISABLED)?.unwrap_or(false),
            required: bool_property(&node, id, PropertyId::REQUIRED)?.unwrap_or(false),
            invalid: bool_property(&node, id, PropertyId::INVALID)?.unwrap_or(false),
            modal: bool_property(&node, id, PropertyId::MODAL)?.unwrap_or(false),
            checked: bool_property(&node, id, PropertyId::CHECKED)?,
            selected: bool_property(&node, id, PropertyId::SELECTED)?,
            expanded: bool_property(&node, id, PropertyId::EXPANDED)?,
            pressed: bool_property(&node, id, PropertyId::PRESSED)?,
            current: current_property(&node, id)?,
        };
        let mut actions = actions(primitive, &role, state.disabled);
        if !state.disabled && bool_property(&node, id, PropertyId::FOCUSABLE)?.unwrap_or(false) {
            actions.focus = true;
        }
        self.nodes.insert(
            id,
            AccessibilityNode {
                id,
                role,
                name,
                description,
                value,
                bounds,
                state,
                actions,
                children,
            },
        );
        Ok(Some(id))
    }

    fn collect_descendant_text(
        &self,
        id: NodeId,
        depth: usize,
        output: &mut String,
    ) -> Result<(), AccessibilityError> {
        if depth >= self.limits.max_depth {
            return Err(AccessibilityError::DepthLimitExceeded);
        }
        let node = self
            .tree
            .node(id)
            .ok_or(AccessibilityError::MissingNode(id))?;
        if node.kind != NodeKind::Text
            && (bool_property(&node, id, PropertyId::HIDDEN)?.unwrap_or(false)
                || bool_property(&node, id, PropertyId::ACCESSIBILITY_HIDDEN)?.unwrap_or(false))
        {
            return Ok(());
        }
        if node.kind == NodeKind::Text {
            output.push_str(&node.text);
            if output.len() > self.limits.max_text_bytes {
                return Err(AccessibilityError::TextLimitExceeded);
            }
            return Ok(());
        }
        for child in node.children {
            self.collect_descendant_text(child, depth + 1, output)?;
        }
        Ok(())
    }

    fn charge_text(&mut self, bytes: usize) -> Result<(), AccessibilityError> {
        self.text_bytes = self
            .text_bytes
            .checked_add(bytes)
            .ok_or(AccessibilityError::TextLimitExceeded)?;
        if self.text_bytes > self.limits.max_text_bytes {
            return Err(AccessibilityError::TextLimitExceeded);
        }
        Ok(())
    }
}

fn role(
    node: &NodeSnapshot,
    primitive: Primitive,
) -> Result<AccessibilityRole, AccessibilityError> {
    if let Some(value) = text_property(node, node.id, PropertyId::ROLE)? {
        return Ok(match value.as_str() {
            "button" => AccessibilityRole::Button,
            "textbox" => AccessibilityRole::TextBox,
            "switch" => AccessibilityRole::Switch,
            "slider" => AccessibilityRole::Slider,
            "img" | "image" => AccessibilityRole::Image,
            "alert" => AccessibilityRole::Alert,
            "dialog" => AccessibilityRole::Dialog,
            "heading" => AccessibilityRole::Heading,
            "paragraph" => AccessibilityRole::Paragraph,
            "status" => AccessibilityRole::Status,
            "progressbar" => AccessibilityRole::ProgressIndicator,
            "separator" => AccessibilityRole::Separator,
            "link" => AccessibilityRole::Link,
            "navigation" => AccessibilityRole::Navigation,
            "toolbar" => AccessibilityRole::Toolbar,
            "list" => AccessibilityRole::List,
            "listitem" => AccessibilityRole::ListItem,
            "radiogroup" => AccessibilityRole::RadioGroup,
            "radio" => AccessibilityRole::RadioButton,
            "combobox" => AccessibilityRole::ComboBox,
            "listbox" => AccessibilityRole::ListBox,
            "option" => AccessibilityRole::Option,
            "alertdialog" => AccessibilityRole::AlertDialog,
            "tooltip" => AccessibilityRole::Tooltip,
            "menubar" => AccessibilityRole::MenuBar,
            "menu" => AccessibilityRole::Menu,
            "menuitem" => AccessibilityRole::MenuItem,
            "menuitemcheckbox" => AccessibilityRole::MenuItemCheckBox,
            "menuitemradio" => AccessibilityRole::MenuItemRadio,
            "tablist" => AccessibilityRole::TabList,
            "tab" => AccessibilityRole::Tab,
            "tabpanel" => AccessibilityRole::TabPanel,
            "grid" => AccessibilityRole::Grid,
            "row" => AccessibilityRole::Row,
            "gridcell" | "cell" => AccessibilityRole::GridCell,
            "columnheader" => AccessibilityRole::ColumnHeader,
            "rowheader" => AccessibilityRole::RowHeader,
            "tree" => AccessibilityRole::Tree,
            "treeitem" => AccessibilityRole::TreeItem,
            "group" => AccessibilityRole::Group,
            "presentation" | "none" => AccessibilityRole::Presentation,
            value => AccessibilityRole::Custom(value.to_string()),
        });
    }
    Ok(match primitive {
        Primitive::Root => AccessibilityRole::Root,
        Primitive::Text => AccessibilityRole::StaticText,
        Primitive::Button => AccessibilityRole::Button,
        Primitive::TextInput | Primitive::TextArea => AccessibilityRole::TextBox,
        Primitive::Toggle => AccessibilityRole::Switch,
        Primitive::Slider => AccessibilityRole::Slider,
        Primitive::Image => AccessibilityRole::Image,
        _ => AccessibilityRole::Group,
    })
}

fn role_uses_descendant_text(role: &AccessibilityRole) -> bool {
    matches!(
        role,
        AccessibilityRole::StaticText
            | AccessibilityRole::Button
            | AccessibilityRole::Switch
            | AccessibilityRole::Alert
            | AccessibilityRole::Dialog
            | AccessibilityRole::AlertDialog
            | AccessibilityRole::Heading
            | AccessibilityRole::Status
            | AccessibilityRole::Link
            | AccessibilityRole::RadioButton
            | AccessibilityRole::Option
            | AccessibilityRole::MenuItem
            | AccessibilityRole::MenuItemCheckBox
            | AccessibilityRole::MenuItemRadio
            | AccessibilityRole::Tab
            | AccessibilityRole::GridCell
            | AccessibilityRole::ColumnHeader
            | AccessibilityRole::RowHeader
            | AccessibilityRole::TreeItem
    )
}

fn actions(primitive: Primitive, role: &AccessibilityRole, disabled: bool) -> AccessibilityActions {
    if disabled {
        return AccessibilityActions::default();
    }
    AccessibilityActions {
        focus: matches!(
            role,
            AccessibilityRole::Button
                | AccessibilityRole::TextBox
                | AccessibilityRole::Switch
                | AccessibilityRole::Slider
                | AccessibilityRole::Dialog
                | AccessibilityRole::AlertDialog
                | AccessibilityRole::Link
                | AccessibilityRole::RadioButton
                | AccessibilityRole::ComboBox
                | AccessibilityRole::Option
                | AccessibilityRole::MenuItem
                | AccessibilityRole::MenuItemCheckBox
                | AccessibilityRole::MenuItemRadio
                | AccessibilityRole::Tab
                | AccessibilityRole::GridCell
                | AccessibilityRole::ColumnHeader
                | AccessibilityRole::RowHeader
                | AccessibilityRole::TreeItem
        ),
        invoke: matches!(
            role,
            AccessibilityRole::Button
                | AccessibilityRole::Link
                | AccessibilityRole::RadioButton
                | AccessibilityRole::Option
                | AccessibilityRole::MenuItem
                | AccessibilityRole::MenuItemCheckBox
                | AccessibilityRole::MenuItemRadio
                | AccessibilityRole::Tab
        ),
        set_value: primitive == Primitive::TextInput
            || primitive == Primitive::TextArea
            || primitive == Primitive::Slider,
        toggle: primitive == Primitive::Toggle
            || matches!(
                role,
                AccessibilityRole::MenuItemCheckBox | AccessibilityRole::MenuItemRadio
            ),
    }
}

fn current_property(
    node: &NodeSnapshot,
    id: NodeId,
) -> Result<Option<AccessibilityCurrent>, AccessibilityError> {
    let Some(value) = text_property(node, id, PropertyId::CURRENT)? else {
        return Ok(None);
    };
    match value.as_str() {
        "false" => Ok(Some(AccessibilityCurrent::False)),
        "true" => Ok(Some(AccessibilityCurrent::True)),
        "page" => Ok(Some(AccessibilityCurrent::Page)),
        "step" => Ok(Some(AccessibilityCurrent::Step)),
        "location" => Ok(Some(AccessibilityCurrent::Location)),
        "date" => Ok(Some(AccessibilityCurrent::Date)),
        "time" => Ok(Some(AccessibilityCurrent::Time)),
        _ => Err(AccessibilityError::InvalidProperty(id, PropertyId::CURRENT)),
    }
}

fn text_property(
    node: &NodeSnapshot,
    id: NodeId,
    property: PropertyId,
) -> Result<Option<String>, AccessibilityError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    match value {
        Value::Text(value) => Ok(Some(value.clone())),
        _ => Err(AccessibilityError::InvalidProperty(id, property)),
    }
}

fn accessible_value_property(
    node: &NodeSnapshot,
    id: NodeId,
) -> Result<String, AccessibilityError> {
    match node.properties.get(&PropertyId::VALUE) {
        None => Ok(String::new()),
        Some(Value::Text(value)) => Ok(value.clone()),
        Some(Value::I64(value)) => Ok(value.to_string()),
        Some(Value::F64(value)) if value.is_finite() => Ok(value.to_string()),
        Some(_) => Err(AccessibilityError::InvalidProperty(id, PropertyId::VALUE)),
    }
}

fn bool_property(
    node: &NodeSnapshot,
    id: NodeId,
    property: PropertyId,
) -> Result<Option<bool>, AccessibilityError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    match value {
        Value::Bool(value) => Ok(Some(*value)),
        _ => Err(AccessibilityError::InvalidProperty(id, property)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_ui_core::{Length, Property};
    use vo_ui_layout::{compute_layout, ApproximateTextMeasurer, LayoutLimits};
    use vo_ui_protocol::{Mutation, MutationBatch, NodeKind, ProtocolLimits};

    fn fixture() -> (TreeMirror, LayoutSnapshot, NodeId, NodeId, NodeId) {
        let root = NodeId::new(0, 1);
        let column = NodeId::new(1, 1);
        let label = NodeId::new(2, 1);
        let characters = NodeId::new(3, 1);
        let input = NodeId::new(4, 1);
        let mut tree = TreeMirror::new(9, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            9,
            1,
            alloc::vec![
                Mutation::Create {
                    id: column,
                    kind: NodeKind::Element(Primitive::Column),
                },
                Mutation::Create {
                    id: label,
                    kind: NodeKind::Element(Primitive::Text),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::ROLE, "alert"),
                },
                Mutation::Create {
                    id: characters,
                    kind: NodeKind::Text,
                },
                Mutation::SetText {
                    id: characters,
                    text: "Name is required".into(),
                },
                Mutation::Create {
                    id: input,
                    kind: NodeKind::Element(Primitive::TextInput),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::ACCESSIBLE_NAME, "Display name"),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(
                        PropertyId::ACCESSIBLE_DESCRIPTION,
                        "Name is required",
                    ),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::VALUE, "Ada"),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::REQUIRED, true),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::INVALID, true),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::WIDTH, Value::Length(Length::Px(120.0))),
                },
                Mutation::InsertBefore {
                    parent: label,
                    child: characters,
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: column,
                    child: label,
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: column,
                    child: input,
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: column,
                    before: None,
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        (tree, layout, label, characters, input)
    }

    #[test]
    fn semantic_tree_flattens_character_nodes_and_preserves_form_state() {
        let (tree, layout, label, characters, input) = fixture();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        assert_eq!(semantics.revision, 1);
        assert_eq!(semantics.len(), 4);
        assert!(semantics.get(characters).is_none());

        let alert = semantics.get(label).unwrap();
        assert_eq!(alert.role, AccessibilityRole::Alert);
        assert_eq!(alert.name, "Name is required");
        let field = semantics.get(input).unwrap();
        assert_eq!(field.role, AccessibilityRole::TextBox);
        assert_eq!(field.name, "Display name");
        assert_eq!(field.description, "Name is required");
        assert_eq!(field.value, "Ada");
        assert!(field.state.required);
        assert!(field.state.invalid);
        assert!(field.actions.focus);
        assert!(field.actions.set_value);
    }

    #[test]
    fn revision_and_text_limits_fail_closed() {
        let (mut tree, layout, _, _, input) = fixture();
        assert_eq!(
            build_accessibility_tree(
                &tree,
                &layout,
                AccessibilityLimits {
                    max_text_bytes: 1,
                    ..AccessibilityLimits::default()
                },
            ),
            Err(AccessibilityError::TextLimitExceeded)
        );
        tree.apply(&MutationBatch::new(
            9,
            2,
            alloc::vec![Mutation::SetProperty {
                id: input,
                property: Property::new(PropertyId::INVALID, false),
            }],
        ))
        .unwrap();
        assert_eq!(
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()),
            Err(AccessibilityError::RevisionMismatch)
        );
    }

    #[test]
    fn modal_dialog_state_is_preserved_for_platform_adapters() {
        let (mut tree, _, label, _, _) = fixture();
        let column = tree.node(label).unwrap().parent.unwrap();
        tree.apply(&MutationBatch::new(
            9,
            2,
            alloc::vec![
                Mutation::SetProperty {
                    id: column,
                    property: Property::new(PropertyId::ROLE, "dialog"),
                },
                Mutation::SetProperty {
                    id: column,
                    property: Property::new(PropertyId::MODAL, true),
                },
                Mutation::SetProperty {
                    id: column,
                    property: Property::new(PropertyId::ACCESSIBLE_NAME, "Confirm action"),
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        let dialog = semantics.get(column).unwrap();
        assert_eq!(dialog.role, AccessibilityRole::Dialog);
        assert_eq!(dialog.name, "Confirm action");
        assert!(dialog.state.modal);
        assert!(dialog.actions.focus);
    }

    #[test]
    fn progress_indicator_preserves_its_accessible_value() {
        let (mut tree, _, label, _, _) = fixture();
        let column = tree.node(label).unwrap().parent.unwrap();
        tree.apply(&MutationBatch::new(
            9,
            2,
            alloc::vec![
                Mutation::SetProperty {
                    id: column,
                    property: Property::new(PropertyId::ROLE, "progressbar"),
                },
                Mutation::SetProperty {
                    id: column,
                    property: Property::new(PropertyId::ACCESSIBLE_NAME, "Download"),
                },
                Mutation::SetProperty {
                    id: column,
                    property: Property::new(PropertyId::VALUE, "42%"),
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        let progress = semantics.get(column).unwrap();
        assert_eq!(progress.role, AccessibilityRole::ProgressIndicator);
        assert_eq!(progress.name, "Download");
        assert_eq!(progress.value, "42%");
    }

    #[test]
    fn slider_projects_numeric_value_and_set_value_action() {
        let root = NodeId::new(0, 1);
        let slider = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(10, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            10,
            1,
            alloc::vec![
                Mutation::Create {
                    id: slider,
                    kind: NodeKind::Element(Primitive::Slider),
                },
                Mutation::SetProperty {
                    id: slider,
                    property: Property::new(PropertyId::ACCESSIBLE_NAME, "Quality"),
                },
                Mutation::SetProperty {
                    id: slider,
                    property: Property::new(PropertyId::VALUE, 42.5_f64),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: slider,
                    before: None,
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        let range = semantics.get(slider).unwrap();
        assert_eq!(range.role, AccessibilityRole::Slider);
        assert_eq!(range.name, "Quality");
        assert_eq!(range.value, "42.5");
        assert!(range.actions.focus);
        assert!(range.actions.set_value);
    }

    #[test]
    fn composite_widget_roles_and_states_survive_projection() {
        let (mut tree, _, label, _, _) = fixture();
        let column = tree.node(label).unwrap().parent.unwrap();
        tree.apply(&MutationBatch::new(
            9,
            2,
            alloc::vec![
                Mutation::SetProperty {
                    id: column,
                    property: Property::new(PropertyId::ROLE, "tablist"),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::ROLE, "tab"),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::SELECTED, true),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::EXPANDED, false),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::PRESSED, true),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::CURRENT, "page"),
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        assert_eq!(
            semantics.get(column).unwrap().role,
            AccessibilityRole::TabList
        );
        let tab = semantics.get(label).unwrap();
        assert_eq!(tab.role, AccessibilityRole::Tab);
        assert_eq!(tab.state.selected, Some(true));
        assert_eq!(tab.state.expanded, Some(false));
        assert_eq!(tab.state.pressed, Some(true));
        assert_eq!(tab.state.current, Some(AccessibilityCurrent::Page));
        assert!(tab.actions.focus);
        assert!(tab.actions.invoke);
    }

    #[test]
    fn composed_controls_can_opt_into_focus_actions() {
        let (mut tree, _layout, label, _, _) = fixture();
        tree.apply(&MutationBatch::new(
            9,
            2,
            alloc::vec![
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::ROLE, "button"),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::FOCUSABLE, true),
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        let button = semantics.get(label).unwrap();
        assert_eq!(button.role, AccessibilityRole::Button);
        assert!(button.actions.focus);
        assert!(button.actions.invoke);

        assert_eq!(layout.revision, 2);
    }

    #[test]
    fn data_collection_roles_preserve_names_and_selection() {
        let (mut tree, _, label, _, _) = fixture();
        let row = tree.node(label).unwrap().parent.unwrap();
        tree.apply(&MutationBatch::new(
            9,
            2,
            alloc::vec![
                Mutation::SetProperty {
                    id: row,
                    property: Property::new(PropertyId::ROLE, "row"),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::ROLE, "gridcell"),
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::SELECTED, true),
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        assert_eq!(semantics.get(row).unwrap().role, AccessibilityRole::Row);
        let cell = semantics.get(label).unwrap();
        assert_eq!(cell.role, AccessibilityRole::GridCell);
        assert_eq!(cell.name, "Name is required");
        assert_eq!(cell.state.selected, Some(true));
        assert!(cell.actions.focus);
    }

    #[test]
    fn hidden_subtree_is_absent_from_accessibility_projection() {
        let (mut tree, _, label, characters, input) = fixture();
        tree.apply(&MutationBatch::new(
            9,
            2,
            alloc::vec![Mutation::SetProperty {
                id: label,
                property: Property::new(PropertyId::HIDDEN, true),
            }],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        assert!(semantics.get(label).is_none());
        assert!(semantics.get(characters).is_none());
        assert!(semantics.get(input).is_some());

        tree.apply(&MutationBatch::new(
            9,
            3,
            alloc::vec![
                Mutation::RemoveProperty {
                    id: label,
                    property: PropertyId::HIDDEN
                },
                Mutation::SetProperty {
                    id: label,
                    property: Property::new(PropertyId::ACCESSIBILITY_HIDDEN, true),
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert!(layout.get(label).is_some());
        let semantics =
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap();
        assert!(semantics.get(label).is_none());
        assert!(semantics.get(characters).is_none());
        assert!(semantics.get(input).is_some());
    }
}
