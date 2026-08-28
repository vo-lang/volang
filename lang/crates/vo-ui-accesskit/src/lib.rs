//! AccessKit bridge for the renderer-neutral Volang UI accessibility tree.

use std::collections::BTreeMap;
use std::fmt;

use accesskit::{
    Action, ActionData, ActionRequest, AriaCurrent, Invalid, Node, NodeId as AccessNodeId,
    Rect as AccessRect, Role, Toggled, Tree, TreeId, TreeUpdate,
};
use vo_ui_accessibility::{
    AccessibilityCurrent, AccessibilityNode, AccessibilityRole, AccessibilityTree,
};
use vo_ui_core::NodeId;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AccessKitBridgeConfig {
    pub max_nodes: usize,
}

impl Default for AccessKitBridgeConfig {
    fn default() -> Self {
        Self { max_nodes: 100_000 }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AccessKitBridgeError {
    InvalidConfig,
    NotInitialized,
    NodeLimitExceeded,
    StaleRevision { current: u64, candidate: u64 },
    UnknownFocus(NodeId),
    ForeignTree,
    InvalidNodeIdentity,
    UnknownActionNode(NodeId),
    UnsupportedAction(NodeId, Action),
    MissingActionData(NodeId, Action),
    InvalidActionData(NodeId, Action),
}

impl fmt::Display for AccessKitBridgeError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "Volang UI AccessKit bridge error: {self:?}")
    }
}

impl std::error::Error for AccessKitBridgeError {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NativeAccessibilityAction {
    Focus { node: NodeId },
    Invoke { node: NodeId },
    SetValue { node: NodeId, value: String },
    Toggle { node: NodeId },
}

#[cfg(any(
    target_os = "macos",
    target_os = "windows",
    all(unix, not(target_vendor = "apple"))
))]
mod platform_state {
    use std::collections::VecDeque;
    use std::sync::{Arc, Mutex};

    #[cfg(all(unix, not(target_vendor = "apple")))]
    use accesskit::DeactivationHandler;
    use accesskit::{ActionHandler, ActionRequest, ActivationHandler, TreeUpdate};

    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub enum PlatformAccessKitError {
        InvalidConfig,
        InvalidFullTree,
        InvalidBounds,
        StatePoisoned,
    }

    pub(super) struct State {
        latest_full: TreeUpdate,
        actions: VecDeque<ActionRequest>,
        max_pending_actions: usize,
        dropped_actions: u64,
        #[cfg(all(unix, not(target_vendor = "apple")))]
        deactivation_count: u64,
    }

    pub(super) type SharedState = Arc<Mutex<State>>;

    pub(super) fn create(
        initial_full: TreeUpdate,
        max_pending_actions: usize,
    ) -> Result<SharedState, PlatformAccessKitError> {
        if max_pending_actions == 0 {
            return Err(PlatformAccessKitError::InvalidConfig);
        }
        validate_full_tree(&initial_full)?;
        Ok(Arc::new(Mutex::new(State {
            latest_full: initial_full,
            actions: VecDeque::with_capacity(max_pending_actions),
            max_pending_actions,
            dropped_actions: 0,
            #[cfg(all(unix, not(target_vendor = "apple")))]
            deactivation_count: 0,
        })))
    }

    pub(super) fn replace_latest_full(
        state: &SharedState,
        latest_full: TreeUpdate,
    ) -> Result<(), PlatformAccessKitError> {
        validate_full_tree(&latest_full)?;
        state
            .lock()
            .map_err(|_| PlatformAccessKitError::StatePoisoned)?
            .latest_full = latest_full;
        Ok(())
    }

    pub(super) fn drain_actions(
        state: &SharedState,
        max: usize,
    ) -> Result<Vec<ActionRequest>, PlatformAccessKitError> {
        let mut state = state
            .lock()
            .map_err(|_| PlatformAccessKitError::StatePoisoned)?;
        let count = max.min(state.actions.len());
        Ok(state.actions.drain(..count).collect())
    }

    pub(super) fn dropped_actions(state: &SharedState) -> Result<u64, PlatformAccessKitError> {
        Ok(state
            .lock()
            .map_err(|_| PlatformAccessKitError::StatePoisoned)?
            .dropped_actions)
    }

    #[cfg(all(unix, not(target_vendor = "apple")))]
    pub(super) fn deactivation_count(state: &SharedState) -> Result<u64, PlatformAccessKitError> {
        Ok(state
            .lock()
            .map_err(|_| PlatformAccessKitError::StatePoisoned)?
            .deactivation_count)
    }

    fn validate_full_tree(update: &TreeUpdate) -> Result<(), PlatformAccessKitError> {
        if update.tree.is_none() {
            return Err(PlatformAccessKitError::InvalidFullTree);
        }
        Ok(())
    }

    pub(super) struct Activation {
        pub(super) state: SharedState,
    }

    impl ActivationHandler for Activation {
        fn request_initial_tree(&mut self) -> Option<TreeUpdate> {
            self.state
                .lock()
                .ok()
                .map(|state| state.latest_full.clone())
        }
    }

    pub(super) struct Actions {
        pub(super) state: SharedState,
    }

    impl ActionHandler for Actions {
        fn do_action(&mut self, request: ActionRequest) {
            let Ok(mut state) = self.state.lock() else {
                return;
            };
            if state.actions.len() == state.max_pending_actions {
                state.dropped_actions = state.dropped_actions.saturating_add(1);
                return;
            }
            state.actions.push_back(request);
        }
    }

    #[cfg(all(unix, not(target_vendor = "apple")))]
    pub(super) struct Deactivation {
        pub(super) state: SharedState,
    }

    #[cfg(all(unix, not(target_vendor = "apple")))]
    impl DeactivationHandler for Deactivation {
        fn deactivate_accessibility(&mut self) {
            let Ok(mut state) = self.state.lock() else {
                return;
            };
            state.deactivation_count = state.deactivation_count.saturating_add(1);
        }
    }
}

#[cfg(any(
    target_os = "macos",
    target_os = "windows",
    all(unix, not(target_vendor = "apple"))
))]
pub use platform_state::PlatformAccessKitError;

/// Retains the last AccessKit projection and emits only changed nodes after
/// the initial full tree. Platform adapters consume `TreeUpdate` atomically.
#[derive(Clone)]
pub struct AccessKitBridge {
    config: AccessKitBridgeConfig,
    revision: u64,
    initialized: bool,
    nodes: BTreeMap<AccessNodeId, Node>,
    source_nodes: BTreeMap<NodeId, vo_ui_accessibility::AccessibilityActions>,
    root: Option<NodeId>,
}

impl AccessKitBridge {
    pub fn new(config: AccessKitBridgeConfig) -> Result<Self, AccessKitBridgeError> {
        if config.max_nodes == 0 {
            return Err(AccessKitBridgeError::InvalidConfig);
        }
        Ok(Self {
            config,
            revision: 0,
            initialized: false,
            nodes: BTreeMap::new(),
            source_nodes: BTreeMap::new(),
            root: None,
        })
    }

    pub const fn revision(&self) -> u64 {
        self.revision
    }

    pub fn update(
        &mut self,
        tree: &AccessibilityTree,
        focused: Option<NodeId>,
    ) -> Result<TreeUpdate, AccessKitBridgeError> {
        if tree.len() > self.config.max_nodes {
            return Err(AccessKitBridgeError::NodeLimitExceeded);
        }
        if tree.revision < self.revision {
            return Err(AccessKitBridgeError::StaleRevision {
                current: self.revision,
                candidate: tree.revision,
            });
        }
        let focused = focused.unwrap_or(tree.root);
        if tree.get(focused).is_none() {
            return Err(AccessKitBridgeError::UnknownFocus(focused));
        }
        let mut projected = BTreeMap::new();
        let mut source_nodes = BTreeMap::new();
        for source in tree.iter() {
            let id = access_node_id(source.id);
            projected.insert(id, project_node(source));
            source_nodes.insert(source.id, source.actions);
        }
        let nodes = projected
            .iter()
            .filter(|(id, node)| self.nodes.get(id) != Some(*node))
            .map(|(id, node)| (*id, node.clone()))
            .collect();
        let tree_metadata = (!self.initialized).then(|| tree_metadata(tree.root));
        self.revision = tree.revision;
        self.initialized = true;
        self.nodes = projected;
        self.source_nodes = source_nodes;
        self.root = Some(tree.root);
        Ok(TreeUpdate {
            nodes,
            tree: tree_metadata,
            tree_id: TreeId::ROOT,
            focus: access_node_id(focused),
        })
    }

    pub fn full_update(&self, focused: Option<NodeId>) -> Result<TreeUpdate, AccessKitBridgeError> {
        let root = self.root.ok_or(AccessKitBridgeError::NotInitialized)?;
        let focused = focused.unwrap_or(root);
        if !self.source_nodes.contains_key(&focused) {
            return Err(AccessKitBridgeError::UnknownFocus(focused));
        }
        Ok(TreeUpdate {
            nodes: self
                .nodes
                .iter()
                .map(|(id, node)| (*id, node.clone()))
                .collect(),
            tree: Some(tree_metadata(root)),
            tree_id: TreeId::ROOT,
            focus: access_node_id(focused),
        })
    }

    pub fn decode_action(
        &self,
        request: &ActionRequest,
    ) -> Result<NativeAccessibilityAction, AccessKitBridgeError> {
        if request.target_tree != TreeId::ROOT {
            return Err(AccessKitBridgeError::ForeignTree);
        }
        let node = source_node_id(request.target_node)?;
        let actions = self
            .source_nodes
            .get(&node)
            .ok_or(AccessKitBridgeError::UnknownActionNode(node))?;
        match request.action {
            Action::Focus if actions.focus => Ok(NativeAccessibilityAction::Focus { node }),
            Action::Click if actions.invoke => Ok(NativeAccessibilityAction::Invoke { node }),
            Action::Click if actions.toggle => Ok(NativeAccessibilityAction::Toggle { node }),
            Action::SetValue if actions.set_value => match request.data.as_ref() {
                Some(ActionData::Value(value)) => Ok(NativeAccessibilityAction::SetValue {
                    node,
                    value: value.to_string(),
                }),
                None => Err(AccessKitBridgeError::MissingActionData(
                    node,
                    request.action,
                )),
                Some(_) => Err(AccessKitBridgeError::InvalidActionData(
                    node,
                    request.action,
                )),
            },
            _ => Err(AccessKitBridgeError::UnsupportedAction(
                node,
                request.action,
            )),
        }
    }
}

fn tree_metadata(root: NodeId) -> Tree {
    let mut metadata = Tree::new(access_node_id(root));
    metadata.toolkit_name = Some(String::from("Volang UI"));
    metadata.toolkit_version = Some(String::from(env!("CARGO_PKG_VERSION")));
    metadata
}

fn project_node(source: &AccessibilityNode) -> Node {
    let mut node = Node::new(role(source));
    if !source.name.is_empty() {
        if source.role == AccessibilityRole::StaticText {
            node.set_value(source.name.clone());
        } else {
            node.set_label(source.name.clone());
        }
    }
    if !source.description.is_empty() {
        node.set_description(source.description.clone());
    }
    if !source.value.is_empty() {
        node.set_value(source.value.clone());
    }
    node.set_bounds(AccessRect {
        x0: source.bounds.x,
        y0: source.bounds.y,
        x1: source.bounds.x + source.bounds.width,
        y1: source.bounds.y + source.bounds.height,
    });
    node.set_children(
        source
            .children
            .iter()
            .copied()
            .map(access_node_id)
            .collect::<Vec<_>>(),
    );
    if source.state.disabled {
        node.set_disabled();
    }
    if source.state.required {
        node.set_required();
    }
    if source.state.invalid {
        node.set_invalid(Invalid::True);
    }
    if source.state.modal {
        node.set_modal();
    }
    if let Some(checked) = source.state.checked {
        node.set_toggled(Toggled::from(checked));
    } else if let Some(pressed) = source.state.pressed {
        node.set_toggled(Toggled::from(pressed));
    }
    if let Some(selected) = source.state.selected {
        node.set_selected(selected);
    }
    if let Some(expanded) = source.state.expanded {
        node.set_expanded(expanded);
    }
    if let Some(current) = source.state.current {
        node.set_aria_current(match current {
            AccessibilityCurrent::False => AriaCurrent::False,
            AccessibilityCurrent::True => AriaCurrent::True,
            AccessibilityCurrent::Page => AriaCurrent::Page,
            AccessibilityCurrent::Step => AriaCurrent::Step,
            AccessibilityCurrent::Location => AriaCurrent::Location,
            AccessibilityCurrent::Date => AriaCurrent::Date,
            AccessibilityCurrent::Time => AriaCurrent::Time,
        });
    }
    if source.actions.focus {
        node.add_action(Action::Focus);
    }
    if source.actions.invoke || source.actions.toggle {
        node.add_action(Action::Click);
    }
    if source.actions.set_value {
        node.add_action(Action::SetValue);
    }
    node
}

fn role(source: &AccessibilityNode) -> Role {
    match source.role {
        AccessibilityRole::Root => Role::Window,
        AccessibilityRole::Group => Role::Group,
        AccessibilityRole::Presentation => Role::GenericContainer,
        AccessibilityRole::StaticText => Role::Label,
        AccessibilityRole::Paragraph => Role::Paragraph,
        AccessibilityRole::Button => Role::Button,
        AccessibilityRole::TextBox => Role::TextInput,
        AccessibilityRole::Switch => Role::Switch,
        AccessibilityRole::Slider => Role::Slider,
        AccessibilityRole::Image => Role::Image,
        AccessibilityRole::Alert => Role::Alert,
        AccessibilityRole::Dialog => Role::Dialog,
        AccessibilityRole::Heading => Role::Heading,
        AccessibilityRole::Status => Role::Status,
        AccessibilityRole::ProgressIndicator => Role::ProgressIndicator,
        AccessibilityRole::Separator => Role::Splitter,
        AccessibilityRole::Link => Role::Link,
        AccessibilityRole::Navigation => Role::Navigation,
        AccessibilityRole::Toolbar => Role::Toolbar,
        AccessibilityRole::List => Role::List,
        AccessibilityRole::ListItem => Role::ListItem,
        AccessibilityRole::RadioGroup => Role::RadioGroup,
        AccessibilityRole::RadioButton => Role::RadioButton,
        AccessibilityRole::ComboBox => Role::ComboBox,
        AccessibilityRole::ListBox => Role::ListBox,
        AccessibilityRole::Option => Role::ListBoxOption,
        AccessibilityRole::AlertDialog => Role::AlertDialog,
        AccessibilityRole::Tooltip => Role::Tooltip,
        AccessibilityRole::MenuBar => Role::MenuBar,
        AccessibilityRole::Menu => Role::Menu,
        AccessibilityRole::MenuItem => Role::MenuItem,
        AccessibilityRole::MenuItemCheckBox => Role::MenuItemCheckBox,
        AccessibilityRole::MenuItemRadio => Role::MenuItemRadio,
        AccessibilityRole::TabList => Role::TabList,
        AccessibilityRole::Tab => Role::Tab,
        AccessibilityRole::TabPanel => Role::TabPanel,
        AccessibilityRole::Grid => Role::Grid,
        AccessibilityRole::Row => Role::Row,
        AccessibilityRole::GridCell => Role::GridCell,
        AccessibilityRole::ColumnHeader => Role::ColumnHeader,
        AccessibilityRole::RowHeader => Role::RowHeader,
        AccessibilityRole::Tree => Role::Tree,
        AccessibilityRole::TreeItem => Role::TreeItem,
        AccessibilityRole::Custom(_) => Role::Group,
    }
}

pub const fn access_node_id(node: NodeId) -> AccessNodeId {
    AccessNodeId((node.generation() as u64) << 32 | node.index() as u64)
}

pub fn source_node_id(node: AccessNodeId) -> Result<NodeId, AccessKitBridgeError> {
    let index = node.0 as u32;
    let generation = (node.0 >> 32) as u32;
    if generation == 0 {
        return Err(AccessKitBridgeError::InvalidNodeIdentity);
    }
    Ok(NodeId::new(index, generation))
}

#[cfg(target_os = "macos")]
mod macos {
    use std::ffi::c_void;

    use accesskit::{ActionRequest, TreeUpdate};
    use accesskit_macos::SubclassingAdapter;

    use super::platform_state::{
        create, drain_actions, dropped_actions, replace_latest_full, Actions, Activation,
        PlatformAccessKitError, SharedState,
    };

    pub type MacOsAccessKitError = PlatformAccessKitError;

    /// Owns the real NSAccessibility adapter attached to a custom AppKit view.
    /// AccessKit supplies the NSAccessibility element objects and notifications.
    pub struct MacOsAccessKitAdapter {
        adapter: SubclassingAdapter,
        state: SharedState,
    }

    impl MacOsAccessKitAdapter {
        /// Installs AccessKit's dynamic NSView subclass before the view is shown.
        ///
        /// # Safety
        ///
        /// `view` must point to a retained NSView that outlives this adapter and
        /// must only be used from the AppKit main thread.
        pub unsafe fn install(
            view: *mut c_void,
            initial_full: TreeUpdate,
            max_pending_actions: usize,
        ) -> Result<Self, MacOsAccessKitError> {
            if view.is_null() {
                return Err(MacOsAccessKitError::InvalidConfig);
            }
            let state = create(initial_full, max_pending_actions)?;
            let adapter = unsafe {
                SubclassingAdapter::new(
                    view,
                    Activation {
                        state: state.clone(),
                    },
                    Actions {
                        state: state.clone(),
                    },
                )
            };
            Ok(Self { adapter, state })
        }

        pub fn update(
            &mut self,
            incremental: TreeUpdate,
            latest_full: TreeUpdate,
        ) -> Result<(), MacOsAccessKitError> {
            replace_latest_full(&self.state, latest_full)?;
            if let Some(events) = self.adapter.update_if_active(|| incremental) {
                events.raise();
            }
            Ok(())
        }

        pub fn update_view_focus_state(&mut self, focused: bool) {
            if let Some(events) = self.adapter.update_view_focus_state(focused) {
                events.raise();
            }
        }

        pub fn drain_actions(
            &mut self,
            max: usize,
        ) -> Result<Vec<ActionRequest>, MacOsAccessKitError> {
            drain_actions(&self.state, max)
        }

        pub fn dropped_actions(&self) -> Result<u64, MacOsAccessKitError> {
            dropped_actions(&self.state)
        }
    }
}

#[cfg(target_os = "macos")]
pub use macos::{MacOsAccessKitAdapter, MacOsAccessKitError};

#[cfg(target_os = "windows")]
mod windows {
    use std::ffi::c_void;

    use accesskit::{ActionRequest, TreeUpdate};
    use accesskit_windows::{SubclassingAdapter, HWND};

    use super::platform_state::{
        create, drain_actions, dropped_actions, replace_latest_full, Actions, Activation,
        PlatformAccessKitError, SharedState,
    };

    pub type WindowsAccessKitError = PlatformAccessKitError;

    /// Owns the real UI Automation adapter attached to a Win32 window.
    pub struct WindowsAccessKitAdapter {
        adapter: SubclassingAdapter,
        state: SharedState,
    }

    impl WindowsAccessKitAdapter {
        /// Installs AccessKit's Win32 subclass before the window is first shown.
        ///
        /// # Safety
        ///
        /// `hwnd` must identify a live window owned by the calling thread. The
        /// window must outlive this adapter and must still be hidden.
        pub unsafe fn install(
            hwnd: *mut c_void,
            initial_full: TreeUpdate,
            max_pending_actions: usize,
        ) -> Result<Self, WindowsAccessKitError> {
            if hwnd.is_null() {
                return Err(WindowsAccessKitError::InvalidConfig);
            }
            let state = create(initial_full, max_pending_actions)?;
            let adapter = SubclassingAdapter::new(
                HWND(hwnd),
                Activation {
                    state: state.clone(),
                },
                Actions {
                    state: state.clone(),
                },
            );
            Ok(Self { adapter, state })
        }

        pub fn update(
            &mut self,
            incremental: TreeUpdate,
            latest_full: TreeUpdate,
        ) -> Result<(), WindowsAccessKitError> {
            replace_latest_full(&self.state, latest_full)?;
            if let Some(events) = self.adapter.update_if_active(|| incremental) {
                events.raise();
            }
            Ok(())
        }

        pub fn drain_actions(
            &mut self,
            max: usize,
        ) -> Result<Vec<ActionRequest>, WindowsAccessKitError> {
            drain_actions(&self.state, max)
        }

        pub fn dropped_actions(&self) -> Result<u64, WindowsAccessKitError> {
            dropped_actions(&self.state)
        }
    }
}

#[cfg(target_os = "windows")]
pub use windows::{WindowsAccessKitAdapter, WindowsAccessKitError};

#[cfg(all(unix, not(target_vendor = "apple")))]
mod unix {
    use accesskit::{ActionRequest, Rect, TreeUpdate};
    use accesskit_unix::Adapter;

    use super::platform_state::{
        create, deactivation_count, drain_actions, dropped_actions, replace_latest_full, Actions,
        Activation, Deactivation, PlatformAccessKitError, SharedState,
    };

    pub type UnixAccessKitError = PlatformAccessKitError;

    /// Owns the real AT-SPI adapter and its bounded cross-thread action queue.
    pub struct UnixAccessKitAdapter {
        adapter: Adapter,
        state: SharedState,
    }

    impl UnixAccessKitAdapter {
        pub fn new(
            initial_full: TreeUpdate,
            max_pending_actions: usize,
        ) -> Result<Self, UnixAccessKitError> {
            let state = create(initial_full, max_pending_actions)?;
            let adapter = Adapter::new(
                Activation {
                    state: state.clone(),
                },
                Actions {
                    state: state.clone(),
                },
                Deactivation {
                    state: state.clone(),
                },
            );
            Ok(Self { adapter, state })
        }

        pub fn update(
            &mut self,
            incremental: TreeUpdate,
            latest_full: TreeUpdate,
        ) -> Result<(), UnixAccessKitError> {
            replace_latest_full(&self.state, latest_full)?;
            self.adapter.update_if_active(|| incremental);
            Ok(())
        }

        pub fn update_window_focus_state(&mut self, focused: bool) {
            self.adapter.update_window_focus_state(focused);
        }

        pub fn set_root_window_bounds(
            &mut self,
            outer: Rect,
            inner: Rect,
        ) -> Result<(), UnixAccessKitError> {
            if !valid_rect(outer) || !valid_rect(inner) {
                return Err(UnixAccessKitError::InvalidBounds);
            }
            self.adapter.set_root_window_bounds(outer, inner);
            Ok(())
        }

        pub fn drain_actions(
            &mut self,
            max: usize,
        ) -> Result<Vec<ActionRequest>, UnixAccessKitError> {
            drain_actions(&self.state, max)
        }

        pub fn dropped_actions(&self) -> Result<u64, UnixAccessKitError> {
            dropped_actions(&self.state)
        }

        pub fn deactivation_count(&self) -> Result<u64, UnixAccessKitError> {
            deactivation_count(&self.state)
        }
    }

    fn valid_rect(rect: Rect) -> bool {
        rect.x0.is_finite()
            && rect.y0.is_finite()
            && rect.x1.is_finite()
            && rect.y1.is_finite()
            && rect.x1 >= rect.x0
            && rect.y1 >= rect.y0
    }
}

#[cfg(all(unix, not(target_vendor = "apple")))]
pub use unix::{UnixAccessKitAdapter, UnixAccessKitError};

#[cfg(test)]
mod tests {
    use super::*;
    use vo_ui_accessibility::{build_accessibility_tree, AccessibilityLimits};
    use vo_ui_core::{HandlerId, Listener, Primitive, Property, PropertyId};
    use vo_ui_layout::{compute_layout, ApproximateTextMeasurer, LayoutLimits, Size};
    use vo_ui_protocol::{Mutation, MutationBatch, NodeKind, ProtocolLimits, TreeMirror};

    fn fixture() -> (AccessibilityTree, NodeId) {
        let root = NodeId::new(0, 1);
        let button = NodeId::new(1, 2);
        let text = NodeId::new(2, 1);
        let mut tree = TreeMirror::new(6, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            6,
            1,
            vec![
                Mutation::Create {
                    id: button,
                    kind: NodeKind::Element(Primitive::Button),
                },
                Mutation::SetProperty {
                    id: button,
                    property: Property::new(PropertyId::ACCESSIBLE_DESCRIPTION, "Runs action"),
                },
                Mutation::Listen {
                    id: button,
                    listener: Listener::new(vo_ui_core::EventType::CLICK, HandlerId::new(4, 1)),
                },
                Mutation::Create {
                    id: text,
                    kind: NodeKind::Text,
                },
                Mutation::SetText {
                    id: text,
                    text: "Save".into(),
                },
                Mutation::InsertBefore {
                    parent: button,
                    child: text,
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: button,
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
        (
            build_accessibility_tree(&tree, &layout, AccessibilityLimits::default()).unwrap(),
            button,
        )
    }

    #[test]
    fn full_projection_preserves_identity_semantics_and_actions() {
        let (tree, button) = fixture();
        let mut bridge = AccessKitBridge::new(AccessKitBridgeConfig::default()).unwrap();
        let update = bridge.update(&tree, Some(button)).unwrap();
        assert_eq!(update.nodes.len(), tree.len());
        assert!(update.tree.is_some());
        assert_eq!(update.focus, access_node_id(button));
        let (_, projected) = update
            .nodes
            .iter()
            .find(|(id, _)| *id == access_node_id(button))
            .unwrap();
        assert_eq!(projected.role(), Role::Button);
        assert_eq!(projected.label(), Some("Save"));
        assert_eq!(projected.description(), Some("Runs action"));
        assert!(projected.supports_action(Action::Click));
        assert_eq!(source_node_id(access_node_id(button)), Ok(button));
    }

    #[test]
    fn unchanged_tree_emits_an_empty_incremental_update() {
        let (tree, button) = fixture();
        let mut bridge = AccessKitBridge::new(AccessKitBridgeConfig::default()).unwrap();
        bridge.update(&tree, Some(button)).unwrap();
        let update = bridge.update(&tree, Some(button)).unwrap();
        assert!(update.nodes.is_empty());
        assert!(update.tree.is_none());
    }

    #[test]
    fn platform_actions_decode_only_when_the_semantic_node_supports_them() {
        let (tree, button) = fixture();
        let mut bridge = AccessKitBridge::new(AccessKitBridgeConfig::default()).unwrap();
        bridge.update(&tree, Some(button)).unwrap();
        let request = ActionRequest {
            action: Action::Click,
            target_tree: TreeId::ROOT,
            target_node: access_node_id(button),
            data: None,
        };
        assert_eq!(
            bridge.decode_action(&request),
            Ok(NativeAccessibilityAction::Invoke { node: button })
        );
        let unsupported = ActionRequest {
            action: Action::SetValue,
            ..request
        };
        assert_eq!(
            bridge.decode_action(&unsupported),
            Err(AccessKitBridgeError::UnsupportedAction(
                button,
                Action::SetValue
            ))
        );
    }

    #[test]
    fn invalid_focus_and_stale_revision_leave_bridge_state_unchanged() {
        let (tree, button) = fixture();
        let mut bridge = AccessKitBridge::new(AccessKitBridgeConfig::default()).unwrap();
        assert_eq!(
            bridge.update(&tree, Some(NodeId::new(99, 1))),
            Err(AccessKitBridgeError::UnknownFocus(NodeId::new(99, 1)))
        );
        assert_eq!(bridge.revision(), 0);
        bridge.update(&tree, Some(button)).unwrap();
        let stale_root = NodeId::new(0, 1);
        let stale_source = TreeMirror::new(6, stale_root, ProtocolLimits::default());
        let stale_layout = compute_layout(
            &stale_source,
            Size::new(320.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let older =
            build_accessibility_tree(&stale_source, &stale_layout, AccessibilityLimits::default())
                .unwrap();
        assert!(matches!(
            bridge.update(&older, Some(button)),
            Err(AccessKitBridgeError::StaleRevision { .. })
        ));
        assert_eq!(bridge.revision(), 1);
    }
}
