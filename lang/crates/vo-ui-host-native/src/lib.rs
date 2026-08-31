use std::collections::{BTreeMap, VecDeque};
use std::time::Duration;

use accesskit::{ActionRequest, TreeUpdate};
use vo_app_host_native::{NativeInputEvent, NativeInputKind, NativeModifiers, NativePointerButton};
use vo_app_protocol::{ViewHandle, WindowHandle};
use vo_ui_accessibility::{
    build_accessibility_tree, AccessibilityError, AccessibilityLimits, AccessibilityTree,
};
use vo_ui_accesskit::{
    AccessKitBridge, AccessKitBridgeConfig, AccessKitBridgeError, NativeAccessibilityAction,
};
use vo_ui_core::{
    CompositionEventData, EventModifiers, EventPayload, EventType, HandlerId, KeyEventData, NodeId,
    PointerEventData, PointerKind, Primitive, PropertyId, ScrollEventData, ScrollUnit,
    TextInputEventData, UiEvent, Value,
};
use vo_ui_desktop::DesktopHost;
use vo_ui_layout::{
    compute_layout, compute_layout_with_scroll_offsets, IntrinsicMeasurer, LayoutError,
    LayoutLimits, LayoutSnapshot, ScrollOffsetProvider, Size,
};
use vo_ui_paint::{
    build_paint_scene_with_interaction, PaintError, PaintInteractionState, PaintLimits, PaintScene,
};
use vo_ui_protocol::{
    ApplyError, EventEnvelope, Mutation, MutationBatch, NodeKind, ProtocolLimits, TreeMirror,
};
use vo_ui_text_native::{NativeTextError, NativeTextSystem, PreparedPaintScene};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NativeUiRect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

impl NativeUiRect {
    pub fn new(x: f64, y: f64, width: f64, height: f64) -> Result<Self, NativeUiHostError> {
        let rect = Self {
            x,
            y,
            width,
            height,
        };
        rect.is_valid()
            .then_some(rect)
            .ok_or(NativeUiHostError::InvalidLayoutRect)
    }

    fn is_valid(self) -> bool {
        self.x.is_finite()
            && self.y.is_finite()
            && self.width.is_finite()
            && self.height.is_finite()
            && self.width >= 0.0
            && self.height >= 0.0
    }

    fn contains(self, x: f64, y: f64) -> bool {
        x >= self.x && y >= self.y && x < self.x + self.width && y < self.y + self.height
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NativeUiLayout {
    pub node: NodeId,
    pub rect: NativeUiRect,
    pub clip: Option<NativeUiRect>,
    pub z_index: i32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeTextSelection {
    pub start_utf16: u32,
    pub length_utf16: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeUiHostConfig {
    pub max_layout_nodes: usize,
    pub max_pending_events: usize,
    pub max_measurement_listeners: usize,
    pub max_measurement_feedback_turns: u8,
    pub wheel_line_milli: u32,
    pub max_overscroll_milli: u32,
    pub momentum_decay_q16: u16,
    pub overscroll_decay_q16: u16,
}

impl Default for NativeUiHostConfig {
    fn default() -> Self {
        Self {
            max_layout_nodes: 100_000,
            max_pending_events: 4_096,
            max_measurement_listeners: 256,
            max_measurement_feedback_turns: 8,
            wheel_line_milli: 40_000,
            max_overscroll_milli: 72_000,
            momentum_decay_q16: 58_982,
            overscroll_decay_q16: 49_152,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NativeUiHostError {
    InvalidConfig,
    Protocol(ApplyError),
    WindowMismatch,
    LayoutRevisionMismatch,
    LayoutLimitExceeded,
    InvalidLayoutRect,
    DuplicateLayoutNode(NodeId),
    MissingLayoutNode(NodeId),
    EventQueueFull,
    EventSequenceExhausted,
    InvalidTextSelectionTarget(NodeId),
    InvalidTextSelectionValue(NodeId),
    InvalidSliderValue(NodeId),
    InvalidModalProperty(NodeId, PropertyId),
    MultipleModalScopes,
    MultipleAutoFocusTargets(NodeId),
    InvalidPointerEvents(NodeId),
    InvalidPointerCapture(NodeId),
    InvalidFlowDirection(NodeId),
    InvalidPortalLayer(NodeId),
    NestedPortal(NodeId),
    InvalidFocusRequest(NodeId),
    MultipleFocusRequests,
    FocusRequestOutsideModal(NodeId),
    MeasurementListenerLimitExceeded,
    MeasurementFeedbackLimitExceeded,
    InvalidMeasurement(NodeId),
    PresentationSequenceExhausted,
    AccessKit(AccessKitBridgeError),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NativeScrollSnapshot {
    pub node: NodeId,
    pub offset_x: f64,
    pub offset_y: f64,
    pub overscroll_x: f64,
    pub overscroll_y: f64,
    pub velocity_x: f64,
    pub velocity_y: f64,
    pub max_offset_x: f64,
    pub max_offset_y: f64,
}

#[derive(Clone, Copy, Debug, Default)]
struct NativeScrollState {
    offset_x: f64,
    offset_y: f64,
    overscroll_x: f64,
    overscroll_y: f64,
    velocity_x: f64,
    velocity_y: f64,
    max_offset_x: f64,
    max_offset_y: f64,
    viewport_width: f64,
    viewport_height: f64,
}

struct MeasurementFeedback {
    measurements: BTreeMap<NodeId, (i64, i64)>,
    events: Vec<(HandlerId, EventType, NodeId, EventPayload)>,
    turns: u8,
}

struct NativeScrollOffsets<'a> {
    states: &'a BTreeMap<NodeId, NativeScrollState>,
}

impl ScrollOffsetProvider for NativeScrollOffsets<'_> {
    fn resolve_scroll_offset(
        &mut self,
        node: NodeId,
        declared_x: f64,
        declared_y: f64,
    ) -> (f64, f64) {
        self.states
            .get(&node)
            .map_or((declared_x, declared_y), |state| {
                (
                    state.offset_x + state.overscroll_x,
                    state.offset_y + state.overscroll_y,
                )
            })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NativeUiLayoutError {
    Layout(LayoutError),
    Host(NativeUiHostError),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NativeUiPresentationError {
    Layout(LayoutError),
    Paint(PaintError),
    Text(NativeTextError),
    Host(NativeUiHostError),
    Accessibility(AccessibilityError),
}

impl core::fmt::Display for NativeUiPresentationError {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Layout(error) => error.fmt(formatter),
            Self::Paint(error) => error.fmt(formatter),
            Self::Text(error) => error.fmt(formatter),
            Self::Host(error) => error.fmt(formatter),
            Self::Accessibility(error) => error.fmt(formatter),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NativeUiPreparedFrame {
    pub layout: LayoutSnapshot,
    pub scene: PaintScene,
    pub presentation: PreparedPaintScene,
    pub accessibility: AccessibilityTree,
    pub accesskit: TreeUpdate,
    pub accesskit_full: TreeUpdate,
}

impl core::fmt::Display for NativeUiLayoutError {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Layout(error) => error.fmt(formatter),
            Self::Host(error) => error.fmt(formatter),
        }
    }
}

impl core::fmt::Display for NativeUiHostError {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(formatter, "native UI host error: {self:?}")
    }
}

/// Retained native host state. The platform window loop drains its bounded
/// input channel into `route_input`, while layout/presentation code publishes
/// one complete bounds snapshot for each accepted UI revision.
pub struct NativeUiHost {
    window: WindowHandle,
    view: ViewHandle,
    session_epoch: u64,
    tree: TreeMirror,
    config: NativeUiHostConfig,
    layouts: BTreeMap<NodeId, NativeUiLayout>,
    events: VecDeque<EventEnvelope>,
    focused: Option<NodeId>,
    active_modal: Option<NodeId>,
    focus_restore: Option<NodeId>,
    modal_focus_order: Vec<NodeId>,
    last_focus_request: Option<(NodeId, i64)>,
    measurements: BTreeMap<NodeId, (i64, i64)>,
    measurement_feedback_turns: u8,
    hovered: Option<NodeId>,
    pressed_pointers: BTreeMap<(u64, NativePointerButton), NodeId>,
    captured_pointers: BTreeMap<u64, NodeId>,
    scroll: BTreeMap<NodeId, NativeScrollState>,
    next_event_sequence: u64,
    next_presentation_sequence: u64,
    accesskit: AccessKitBridge,
}

impl NativeUiHost {
    pub fn new(
        window: WindowHandle,
        view: ViewHandle,
        session_epoch: u64,
        root: NodeId,
        protocol_limits: ProtocolLimits,
        config: NativeUiHostConfig,
    ) -> Result<Self, NativeUiHostError> {
        if !window.is_valid()
            || !view.is_valid()
            || session_epoch == 0
            || config.max_layout_nodes == 0
            || config.max_pending_events == 0
            || config.max_measurement_listeners == 0
            || config.max_measurement_feedback_turns == 0
            || config.wheel_line_milli == 0
            || config.max_overscroll_milli == 0
            || config.momentum_decay_q16 == 0
            || config.overscroll_decay_q16 == 0
        {
            return Err(NativeUiHostError::InvalidConfig);
        }
        let accesskit = AccessKitBridge::new(AccessKitBridgeConfig {
            max_nodes: config.max_layout_nodes,
        })
        .map_err(NativeUiHostError::AccessKit)?;
        Ok(Self {
            window,
            view,
            session_epoch,
            tree: TreeMirror::new(session_epoch, root, protocol_limits),
            config,
            layouts: BTreeMap::new(),
            events: VecDeque::with_capacity(config.max_pending_events),
            focused: None,
            active_modal: None,
            focus_restore: None,
            modal_focus_order: Vec::new(),
            last_focus_request: None,
            measurements: BTreeMap::new(),
            measurement_feedback_turns: 0,
            hovered: None,
            pressed_pointers: BTreeMap::new(),
            captured_pointers: BTreeMap::new(),
            scroll: BTreeMap::new(),
            next_event_sequence: 1,
            next_presentation_sequence: 1,
            accesskit,
        })
    }

    pub const fn revision(&self) -> u64 {
        self.tree.revision()
    }

    pub const fn window(&self) -> WindowHandle {
        self.window
    }

    pub const fn view(&self) -> ViewHandle {
        self.view
    }

    pub const fn session_epoch(&self) -> u64 {
        self.session_epoch
    }

    pub const fn tree(&self) -> &TreeMirror {
        &self.tree
    }

    pub const fn focused_node(&self) -> Option<NodeId> {
        self.focused
    }

    fn paint_interaction(&self) -> PaintInteractionState {
        PaintInteractionState {
            hovered: self.hovered,
            pressed: self.pressed_pointers.values().next().copied(),
            focused: self.focused,
        }
    }

    pub fn scroll_snapshot(&self, node: NodeId) -> Option<NativeScrollSnapshot> {
        self.scroll.get(&node).map(|state| NativeScrollSnapshot {
            node,
            offset_x: state.offset_x,
            offset_y: state.offset_y,
            overscroll_x: state.overscroll_x,
            overscroll_y: state.overscroll_y,
            velocity_x: state.velocity_x,
            velocity_y: state.velocity_y,
            max_offset_x: state.max_offset_x,
            max_offset_y: state.max_offset_y,
        })
    }

    pub fn has_active_scroll_animation(&self) -> bool {
        self.scroll.values().any(|state| {
            state.velocity_x.abs() >= 0.1
                || state.velocity_y.abs() >= 0.1
                || state.overscroll_x.abs() >= 0.1
                || state.overscroll_y.abs() >= 0.1
        })
    }

    /// Advances host-owned momentum and rubber-band recovery. The platform
    /// loop calls this at frame cadence and prepares a new presentation frame
    /// while it returns `true`.
    pub fn advance_scroll_physics(&mut self, elapsed: Duration) -> bool {
        let seconds = elapsed.as_secs_f64().clamp(0.0, 0.05);
        if seconds == 0.0 {
            return self.has_active_scroll_animation();
        }
        let frames = seconds * 60.0;
        let momentum = (f64::from(self.config.momentum_decay_q16) / 65_535.0).powf(frames);
        let recovery = (f64::from(self.config.overscroll_decay_q16) / 65_535.0).powf(frames);
        for state in self.scroll.values_mut() {
            advance_scroll_axis(
                &mut state.offset_x,
                &mut state.overscroll_x,
                &mut state.velocity_x,
                state.max_offset_x,
                seconds,
                momentum,
                recovery,
            );
            advance_scroll_axis(
                &mut state.offset_y,
                &mut state.overscroll_y,
                &mut state.velocity_y,
                state.max_offset_y,
                seconds,
                momentum,
                recovery,
            );
        }
        self.has_active_scroll_animation()
    }

    /// Returns the controlled UTF-16 selection requested by the latest
    /// committed UI revision. Native text-system adapters apply this after
    /// synchronizing the input value for the same revision.
    pub fn controlled_text_selection(
        &self,
        node: NodeId,
    ) -> Result<Option<NativeTextSelection>, NativeUiHostError> {
        controlled_text_selection(&self.tree, node)
    }

    pub fn route_accessibility_action(
        &mut self,
        request: &ActionRequest,
    ) -> Result<bool, NativeUiHostError> {
        let action = self
            .accesskit
            .decode_action(request)
            .map_err(NativeUiHostError::AccessKit)?;
        let node = match &action {
            NativeAccessibilityAction::Focus { node }
            | NativeAccessibilityAction::Invoke { node }
            | NativeAccessibilityAction::SetValue { node, .. }
            | NativeAccessibilityAction::Toggle { node } => *node,
        };
        if self
            .active_modal
            .is_some_and(|modal| !tree_contains(&self.tree, modal, node))
        {
            return Ok(false);
        }
        match action {
            NativeAccessibilityAction::Focus { node } => {
                self.focused = Some(node);
                if let Some((target, handler)) = self.listener_target(node, EventType::FOCUS) {
                    self.enqueue_event(handler, EventType::FOCUS, target, EventPayload::None)?;
                }
            }
            NativeAccessibilityAction::Invoke { node }
            | NativeAccessibilityAction::Toggle { node } => {
                return self.route_semantic_invoke(node);
            }
            NativeAccessibilityAction::SetValue { node, value } => {
                let Some((target, handler)) = self.listener_target(node, EventType::INPUT) else {
                    return Ok(false);
                };
                self.enqueue_event(handler, EventType::INPUT, target, EventPayload::Text(value))?;
            }
        }
        Ok(true)
    }

    /// Invokes one exact committed node through its current activation listener.
    /// Native accessibility adapters and semantic release automation use this
    /// path when pointer coordinates would add an unrelated hit-test step.
    pub fn route_semantic_invoke(&mut self, node: NodeId) -> Result<bool, NativeUiHostError> {
        if self
            .active_modal
            .is_some_and(|modal| !tree_contains(&self.tree, modal, node))
        {
            return Ok(false);
        }
        if let Some((target, handler)) = self.listener_target(node, EventType::CLICK) {
            self.enqueue_event(handler, EventType::CLICK, target, EventPayload::None)?;
            return Ok(true);
        }
        let Some((target, handler, checked)) = self.toggle_change(node) else {
            return Ok(false);
        };
        self.enqueue_event(
            handler,
            EventType::CHANGE,
            target,
            EventPayload::Toggle(checked),
        )?;
        Ok(true)
    }

    fn toggle_change(&self, start: NodeId) -> Option<(NodeId, HandlerId, bool)> {
        let (target, handler) = self.listener_target(start, EventType::CHANGE)?;
        let node = self.tree.node(target)?;
        if node.kind != NodeKind::Element(Primitive::Toggle)
            || node.properties.get(&PropertyId::DISABLED) == Some(&Value::Bool(true))
        {
            return None;
        }
        let checked = node.properties.get(&PropertyId::CHECKED) == Some(&Value::Bool(true));
        Some((target, handler, !checked))
    }

    pub fn set_layout_snapshot(
        &mut self,
        revision: u64,
        layouts: impl IntoIterator<Item = NativeUiLayout>,
    ) -> Result<(), NativeUiHostError> {
        if revision != self.tree.revision() {
            return Err(NativeUiHostError::LayoutRevisionMismatch);
        }
        let mut staged = BTreeMap::new();
        for layout in layouts {
            if staged.len() >= self.config.max_layout_nodes {
                return Err(NativeUiHostError::LayoutLimitExceeded);
            }
            if !layout.rect.is_valid() || layout.clip.is_some_and(|clip| !clip.is_valid()) {
                return Err(NativeUiHostError::InvalidLayoutRect);
            }
            if self.tree.node(layout.node).is_none() {
                return Err(NativeUiHostError::MissingLayoutNode(layout.node));
            }
            if staged.insert(layout.node, layout).is_some() {
                return Err(NativeUiHostError::DuplicateLayoutNode(layout.node));
            }
        }
        self.layouts = staged;
        Ok(())
    }

    pub fn compute_and_set_layout<M: IntrinsicMeasurer>(
        &mut self,
        viewport: Size,
        limits: LayoutLimits,
        measurer: &mut M,
    ) -> Result<LayoutSnapshot, NativeUiLayoutError> {
        let snapshot = compute_layout(&self.tree, viewport, limits, measurer)
            .map_err(NativeUiLayoutError::Layout)?;
        let measurement_feedback = self
            .prepare_measurement_feedback(&snapshot)
            .map_err(NativeUiLayoutError::Host)?;
        let layouts = snapshot.iter().map(|layout| NativeUiLayout {
            node: layout.node,
            rect: NativeUiRect {
                x: layout.rect.x,
                y: layout.rect.y,
                width: layout.rect.width,
                height: layout.rect.height,
            },
            clip: layout.clip.map(|clip| NativeUiRect {
                x: clip.x,
                y: clip.y,
                width: clip.width,
                height: clip.height,
            }),
            z_index: layout.z_index,
        });
        self.set_layout_snapshot(snapshot.revision, layouts)
            .map_err(NativeUiLayoutError::Host)?;
        self.publish_measurement_feedback(measurement_feedback)
            .map_err(NativeUiLayoutError::Host)?;
        Ok(snapshot)
    }

    /// Builds the immutable presentation scene consumed by a GPU or software
    /// presenter. Revision equality is checked by the paint layer, so a host
    /// cannot accidentally render layout from an earlier UI transaction.
    pub fn build_paint_scene(
        &self,
        layout: &LayoutSnapshot,
        limits: PaintLimits,
    ) -> Result<PaintScene, PaintError> {
        build_paint_scene_with_interaction(&self.tree, layout, limits, self.paint_interaction())
    }

    /// Computes, shapes, and rasterizes one complete native frame before
    /// publishing its hit-test bounds. A failure keeps the prior host layout
    /// active, so input can never observe a partially prepared revision.
    pub fn prepare_frame(
        &mut self,
        viewport: Size,
        scale: f32,
        layout_limits: LayoutLimits,
        paint_limits: PaintLimits,
        text: &mut NativeTextSystem,
    ) -> Result<NativeUiPreparedFrame, NativeUiPresentationError> {
        let mut staged_scroll = self.scroll.clone();
        let mut offsets = NativeScrollOffsets {
            states: &staged_scroll,
        };
        let layout = compute_layout_with_scroll_offsets(
            &self.tree,
            viewport,
            layout_limits,
            text,
            &mut offsets,
        )
        .map_err(NativeUiPresentationError::Layout)?;
        let measurement_feedback = self
            .prepare_measurement_feedback(&layout)
            .map_err(NativeUiPresentationError::Host)?;
        if let Some(error) = text.take_measure_error() {
            return Err(NativeUiPresentationError::Text(error));
        }
        sync_scroll_metrics(&mut staged_scroll, &layout);
        let scene = build_paint_scene_with_interaction(
            &self.tree,
            &layout,
            paint_limits,
            self.paint_interaction(),
        )
        .map_err(NativeUiPresentationError::Paint)?;
        let accessibility = build_accessibility_tree(
            &self.tree,
            &layout,
            AccessibilityLimits {
                max_nodes: self.config.max_layout_nodes,
                ..AccessibilityLimits::default()
            },
        )
        .map_err(NativeUiPresentationError::Accessibility)?;
        let mut staged_accesskit = self.accesskit.clone();
        let accesskit = staged_accesskit
            .update(&accessibility, self.focused)
            .map_err(|error| {
                NativeUiPresentationError::Host(NativeUiHostError::AccessKit(error))
            })?;
        let accesskit_full = staged_accesskit
            .full_update(self.focused)
            .map_err(|error| {
                NativeUiPresentationError::Host(NativeUiHostError::AccessKit(error))
            })?;
        let mut presentation = text
            .prepare_scene_with_focus(&scene, scale, self.focused)
            .map_err(NativeUiPresentationError::Text)?;
        let frame_id = self.next_presentation_sequence;
        let next_frame_id = frame_id
            .checked_add(1)
            .ok_or(NativeUiPresentationError::Host(
                NativeUiHostError::PresentationSequenceExhausted,
            ))?;
        presentation.frame_id = frame_id;
        let layouts = layout.iter().map(|item| NativeUiLayout {
            node: item.node,
            rect: NativeUiRect {
                x: item.rect.x,
                y: item.rect.y,
                width: item.rect.width,
                height: item.rect.height,
            },
            clip: item.clip.map(|clip| NativeUiRect {
                x: clip.x,
                y: clip.y,
                width: clip.width,
                height: clip.height,
            }),
            z_index: item.z_index,
        });
        self.set_layout_snapshot(layout.revision, layouts)
            .map_err(NativeUiPresentationError::Host)?;
        self.publish_measurement_feedback(measurement_feedback)
            .map_err(NativeUiPresentationError::Host)?;
        self.scroll = staged_scroll;
        self.next_presentation_sequence = next_frame_id;
        self.accesskit = staged_accesskit;
        Ok(NativeUiPreparedFrame {
            layout,
            scene,
            presentation,
            accessibility,
            accesskit,
            accesskit_full,
        })
    }

    /// Builds a platform-neutral semantic snapshot for NSAccessibility, UIA,
    /// or AT-SPI adapters from the same committed tree and layout revision.
    pub fn build_accessibility_tree(
        &self,
        layout: &LayoutSnapshot,
        limits: AccessibilityLimits,
    ) -> Result<AccessibilityTree, AccessibilityError> {
        build_accessibility_tree(&self.tree, layout, limits)
    }

    /// Converts one native platform event into a bounded set of validated UI
    /// events. Enter and Space additionally invoke focused button primitives,
    /// matching the browser's intrinsic button behavior exactly once.
    /// Listener identity is selected from the latest committed tree and is
    /// validated again by `DesktopRenderer::poll_event` before VM delivery.
    pub fn route_input(&mut self, event: &NativeInputEvent) -> Result<bool, NativeUiHostError> {
        if event.window != self.window || event.view != self.view {
            return Err(NativeUiHostError::WindowMismatch);
        }
        if let NativeInputKind::Key {
            logical_key,
            pressed: true,
            modifiers,
            ..
        } = &event.kind
        {
            if logical_key == "Tab" && self.active_modal.is_some() {
                self.cycle_modal_focus(modifiers.shift);
                return Ok(false);
            }
        }
        if let NativeInputKind::PointerButton {
            device,
            button,
            pressed,
            x_milli,
            y_milli,
            ..
        } = &event.kind
        {
            return self.route_pointer_button(*device, *button, *pressed, *x_milli, *y_milli);
        }
        let slider_dragged = if let NativeInputKind::PointerMoved {
            device,
            x_milli,
            y_milli,
            ..
        } = &event.kind
        {
            self.hovered =
                self.hit_test(f64::from(*x_milli) / 1_000.0, f64::from(*y_milli) / 1_000.0);
            self.route_slider_pointer(*device, f64::from(*x_milli) / 1_000.0)?
        } else {
            false
        };
        let cancelled = match &event.kind {
            NativeInputKind::FocusChanged(false) => {
                self.hovered = None;
                self.cancel_captured_pointers(None)?
            }
            NativeInputKind::DeviceDisconnected { device, .. } => {
                self.hovered = None;
                return self.cancel_captured_pointers(Some(*device));
            }
            _ => false,
        };
        let local_scroll = if let NativeInputKind::Wheel {
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            unit,
            ..
        } = &event.kind
        {
            self.apply_native_wheel(*x_milli, *y_milli, *delta_x_milli, *delta_y_milli, *unit)
        } else {
            false
        };
        let Some((start, event_type, payload)) = self.normalize_input(&event.kind) else {
            return Ok(local_scroll || cancelled || slider_dragged);
        };
        if event_type == EventType::POINTER_DOWN {
            self.focused = Some(start);
        }
        let mut events = Vec::with_capacity(3);
        if let Some((target, handler)) = self.listener_target(start, event_type) {
            events.push((handler, event_type, target, payload));
        }
        if let Some(value) = self.keyboard_text_edit(&event.kind) {
            if let Some((target, handler)) = self.listener_target(start, EventType::INPUT) {
                events.push((
                    handler,
                    EventType::INPUT,
                    target,
                    EventPayload::TextInput(value),
                ));
            }
        }
        if let Some(value) = self.keyboard_text_selection(&event.kind) {
            if let Some((target, handler)) =
                self.listener_target(start, EventType::SELECTION_CHANGE)
            {
                events.push((
                    handler,
                    EventType::SELECTION_CHANGE,
                    target,
                    EventPayload::TextInput(value),
                ));
            }
        }
        if let Some((target, value)) = self.keyboard_slider_edit(&event.kind)? {
            if let Some((target, handler)) = self.listener_target(target, EventType::INPUT) {
                events.push((handler, EventType::INPUT, target, EventPayload::Text(value)));
            }
        }
        if keyboard_activation(&event.kind) {
            if let Some((target, handler)) = self.listener_target(start, EventType::CLICK) {
                if keyboard_activatable(&self.tree, target) {
                    events.push((handler, EventType::CLICK, target, EventPayload::None));
                }
            } else if let Some((target, handler, checked)) = self.toggle_change(start) {
                events.push((
                    handler,
                    EventType::CHANGE,
                    target,
                    EventPayload::Toggle(checked),
                ));
            }
        }
        let queued = !events.is_empty();
        self.enqueue_events(events)?;
        Ok(queued || local_scroll || cancelled || slider_dragged)
    }

    fn slider_ancestor(&self, mut node: NodeId) -> Option<NodeId> {
        loop {
            let current = self.tree.node(node)?;
            if current.kind == NodeKind::Element(Primitive::Slider) {
                return Some(node);
            }
            node = current.parent?;
        }
    }

    fn slider_range(&self, node: NodeId) -> Result<(f64, f64, f64, f64), NativeUiHostError> {
        let snapshot = self
            .tree
            .node(node)
            .ok_or(NativeUiHostError::InvalidSliderValue(node))?;
        let read = |property: PropertyId, fallback: f64| match snapshot.properties.get(&property) {
            None => Ok(fallback),
            Some(Value::F64(value)) if value.is_finite() => Ok(*value),
            Some(Value::I64(value)) => Ok(*value as f64),
            _ => Err(NativeUiHostError::InvalidSliderValue(node)),
        };
        let minimum = read(PropertyId::MIN_VALUE, 0.0)?;
        let maximum = read(PropertyId::MAX_VALUE, 100.0)?;
        let step = read(PropertyId::STEP_VALUE, 1.0)?;
        let value = read(PropertyId::VALUE, minimum)?;
        if maximum <= minimum || step <= 0.0 || value < minimum || value > maximum {
            return Err(NativeUiHostError::InvalidSliderValue(node));
        }
        Ok((value, minimum, maximum, step))
    }

    fn slider_value_at(&self, node: NodeId, x: f64) -> Result<String, NativeUiHostError> {
        let (_, minimum, maximum, step) = self.slider_range(node)?;
        let layout = self
            .layouts
            .get(&node)
            .ok_or(NativeUiHostError::MissingLayoutNode(node))?;
        let ratio = if layout.rect.width <= 0.0 {
            0.0
        } else {
            ((x - layout.rect.x) / layout.rect.width).clamp(0.0, 1.0)
        };
        let raw = minimum + ratio * (maximum - minimum);
        let value = (minimum + ((raw - minimum) / step).round() * step).clamp(minimum, maximum);
        Ok(value.to_string())
    }

    fn route_slider_pointer(&mut self, device: u64, x: f64) -> Result<bool, NativeUiHostError> {
        let Some(target) = self.captured_pointers.get(&device).copied() else {
            return Ok(false);
        };
        let Some(target) = self.slider_ancestor(target) else {
            return Ok(false);
        };
        let Some((target, handler)) = self.listener_target(target, EventType::INPUT) else {
            return Ok(false);
        };
        let value = self.slider_value_at(target, x)?;
        self.enqueue_event(handler, EventType::INPUT, target, EventPayload::Text(value))?;
        Ok(true)
    }

    fn keyboard_slider_edit(
        &self,
        input: &NativeInputKind,
    ) -> Result<Option<(NodeId, String)>, NativeUiHostError> {
        let NativeInputKind::Key {
            logical_key,
            pressed: true,
            ..
        } = input
        else {
            return Ok(None);
        };
        let Some(target) = self.focused.and_then(|node| self.slider_ancestor(node)) else {
            return Ok(None);
        };
        let (value, minimum, maximum, step) = self.slider_range(target)?;
        let next = match logical_key.as_str() {
            "ArrowLeft" | "ArrowDown" => (value - step).max(minimum),
            "ArrowRight" | "ArrowUp" => (value + step).min(maximum),
            "Home" => minimum,
            "End" => maximum,
            _ => return Ok(None),
        };
        Ok(Some((target, next.to_string())))
    }

    fn apply_native_wheel(
        &mut self,
        x_milli: i32,
        y_milli: i32,
        delta_x_milli: i32,
        delta_y_milli: i32,
        unit: vo_app_host_native::NativeScrollUnit,
    ) -> bool {
        let (x, y) = points(x_milli, y_milli);
        let Some(start) = self.hit_test(x, y) else {
            return false;
        };
        let Some(node) = self.nearest_scroll_container(start) else {
            return false;
        };
        let Some(state) = self.scroll.get_mut(&node) else {
            return false;
        };
        let delta_x = f64::from(delta_x_milli) / 1_000.0;
        let delta_y = f64::from(delta_y_milli) / 1_000.0;
        let (delta_x, delta_y) = match unit {
            vo_app_host_native::NativeScrollUnit::Pixel => (delta_x, delta_y),
            vo_app_host_native::NativeScrollUnit::Line => {
                let scale = f64::from(self.config.wheel_line_milli) / 1_000.0;
                (delta_x * scale, delta_y * scale)
            }
            vo_app_host_native::NativeScrollUnit::Page => (
                delta_x * state.viewport_width,
                delta_y * state.viewport_height,
            ),
        };
        let overscroll_limit = f64::from(self.config.max_overscroll_milli) / 1_000.0;
        let moved_x = apply_scroll_impulse(
            &mut state.offset_x,
            &mut state.overscroll_x,
            &mut state.velocity_x,
            state.max_offset_x,
            delta_x,
            overscroll_limit,
        );
        let moved_y = apply_scroll_impulse(
            &mut state.offset_y,
            &mut state.overscroll_y,
            &mut state.velocity_y,
            state.max_offset_y,
            delta_y,
            overscroll_limit,
        );
        moved_x || moved_y
    }

    fn nearest_scroll_container(&self, mut node: NodeId) -> Option<NodeId> {
        loop {
            if self.scroll.contains_key(&node) {
                return Some(node);
            }
            node = self.tree.node(node)?.parent?;
        }
    }

    fn route_pointer_button(
        &mut self,
        device: u64,
        button: NativePointerButton,
        pressed: bool,
        x_milli: i32,
        y_milli: i32,
    ) -> Result<bool, NativeUiHostError> {
        let (x, y) = points(x_milli, y_milli);
        let hit = self.hit_test(x, y);
        self.hovered = hit;
        let captured = self.captured_pointers.get(&device).copied();
        let Some(start) = captured.or(hit) else {
            if !pressed {
                self.pressed_pointers.remove(&(device, button));
                self.captured_pointers.remove(&device);
            }
            return Ok(false);
        };
        let button_index = pointer_button(button);
        let pointer = pointer_payload(
            device,
            x,
            y,
            button_index,
            if pressed && (0..16).contains(&button_index) {
                1_u16 << u32::try_from(button_index).unwrap_or(0)
            } else {
                0
            },
        );
        let mut events = Vec::with_capacity(3);
        let pointer_event = if pressed {
            EventType::POINTER_DOWN
        } else {
            EventType::POINTER_UP
        };
        let pointer_listener = self.listener_target(start, pointer_event);
        let context_listener = if pressed && button == NativePointerButton::Secondary {
            self.listener_target(start, EventType::CONTEXT_MENU)
        } else {
            None
        };
        let focus_target = pointer_listener
            .map(|(target, _)| target)
            .or_else(|| context_listener.map(|(target, _)| target))
            .or_else(|| {
                self.listener_target(start, EventType::CLICK)
                    .map(|(target, _)| target)
            })
            .or_else(|| {
                self.listener_target(start, EventType::INPUT)
                    .map(|(target, _)| target)
            })
            .or_else(|| {
                self.listener_target(start, EventType::CHANGE)
                    .map(|(target, _)| target)
            })
            .unwrap_or(start);
        if let Some((target, handler)) = pointer_listener {
            events.push((handler, pointer_event, target, pointer));
        }
        if let Some((target, handler)) = context_listener {
            events.push((
                handler,
                EventType::CONTEXT_MENU,
                target,
                pointer_payload(device, x, y, button_index, 0),
            ));
        }
        let pressed_target = self.pressed_pointers.get(&(device, button)).copied();
        if !pressed && button == NativePointerButton::Primary && pressed_target == hit {
            if let Some(hit) = hit {
                if let Some((target, handler)) = self.listener_target(hit, EventType::CLICK) {
                    events.push((handler, EventType::CLICK, target, EventPayload::None));
                } else if let Some((target, handler, checked)) = self.toggle_change(hit) {
                    events.push((
                        handler,
                        EventType::CHANGE,
                        target,
                        EventPayload::Toggle(checked),
                    ));
                }
            }
        }
        let mut queued = !events.is_empty();
        self.enqueue_events(events)?;
        if pressed {
            self.focused = Some(focus_target);
            self.pressed_pointers.insert((device, button), start);
            if button == NativePointerButton::Primary {
                if let Some(slider) = self.slider_ancestor(start) {
                    self.captured_pointers.insert(device, slider);
                    if let Some((target, handler)) = self.listener_target(slider, EventType::INPUT)
                    {
                        let value = self.slider_value_at(target, x)?;
                        self.enqueue_event(
                            handler,
                            EventType::INPUT,
                            target,
                            EventPayload::Text(value),
                        )?;
                        queued = true;
                    }
                }
            }
            if let Some((target, _)) = pointer_listener {
                if captures_pointer(&self.tree, target) {
                    self.captured_pointers.insert(device, target);
                }
            }
        } else {
            self.pressed_pointers.remove(&(device, button));
            self.captured_pointers.remove(&device);
        }
        Ok(queued)
    }

    fn cancel_captured_pointers(&mut self, device: Option<u64>) -> Result<bool, NativeUiHostError> {
        let captures = self
            .captured_pointers
            .iter()
            .filter(|(candidate, _)| device.is_none_or(|device| device == **candidate))
            .map(|(device, node)| (*device, *node))
            .collect::<Vec<_>>();
        let mut events = Vec::with_capacity(captures.len());
        for (device, node) in &captures {
            if let Some((target, handler)) = self.listener_target(*node, EventType::POINTER_CANCEL)
            {
                events.push((
                    handler,
                    EventType::POINTER_CANCEL,
                    target,
                    pointer_payload(*device, 0.0, 0.0, -1, 0),
                ));
            }
        }
        let queued = !events.is_empty();
        self.enqueue_events(events)?;
        for (device, _) in captures {
            self.captured_pointers.remove(&device);
            self.pressed_pointers
                .retain(|(pressed_device, _), _| *pressed_device != device);
        }
        Ok(queued)
    }

    /// Queues the trusted renderer-neutral wake used after a worker goroutine
    /// changes component state. Sharing the host sequence allocator with
    /// platform input keeps reverse events strictly monotonic.
    pub fn queue_invalidation(&mut self) -> Result<(), NativeUiHostError> {
        self.enqueue_event(
            HandlerId::new(u32::MAX, 1),
            EventType::INVALIDATE,
            self.tree.root(),
            EventPayload::None,
        )
    }

    fn enqueue_event(
        &mut self,
        handler: HandlerId,
        event: EventType,
        target: NodeId,
        payload: EventPayload,
    ) -> Result<(), NativeUiHostError> {
        self.enqueue_events(vec![(handler, event, target, payload)])
    }

    fn enqueue_events(
        &mut self,
        events: Vec<(HandlerId, EventType, NodeId, EventPayload)>,
    ) -> Result<(), NativeUiHostError> {
        if self.events.len().saturating_add(events.len()) > self.config.max_pending_events {
            return Err(NativeUiHostError::EventQueueFull);
        }
        let count =
            u64::try_from(events.len()).map_err(|_| NativeUiHostError::EventSequenceExhausted)?;
        let next_sequence = self
            .next_event_sequence
            .checked_add(count)
            .ok_or(NativeUiHostError::EventSequenceExhausted)?;
        for (offset, (handler, event, target, payload)) in events.into_iter().enumerate() {
            let offset =
                u64::try_from(offset).map_err(|_| NativeUiHostError::EventSequenceExhausted)?;
            self.events.push_back(EventEnvelope::new(
                self.session_epoch,
                UiEvent {
                    handler,
                    event,
                    target,
                    sequence: self.next_event_sequence + offset,
                    payload,
                },
            ));
        }
        self.next_event_sequence = next_sequence;
        Ok(())
    }

    fn prepare_measurement_feedback(
        &self,
        layout: &LayoutSnapshot,
    ) -> Result<MeasurementFeedback, NativeUiHostError> {
        let mut measurements = BTreeMap::new();
        let mut events = Vec::new();
        for item in layout.iter() {
            let node = self
                .tree
                .node(item.node)
                .ok_or(NativeUiHostError::MissingLayoutNode(item.node))?;
            let Some(listener) = node.listeners.get(&EventType::LAYOUT) else {
                continue;
            };
            if measurements.len() >= self.config.max_measurement_listeners {
                return Err(NativeUiHostError::MeasurementListenerLimitExceeded);
            }
            let width = quantized_measurement(item.rect.width)
                .ok_or(NativeUiHostError::InvalidMeasurement(item.node))?;
            let height = quantized_measurement(item.rect.height)
                .ok_or(NativeUiHostError::InvalidMeasurement(item.node))?;
            measurements.insert(item.node, (width, height));
            if self.measurements.get(&item.node) == Some(&(width, height)) {
                continue;
            }
            events.push((
                listener.handler,
                EventType::LAYOUT,
                item.node,
                EventPayload::Scroll(ScrollEventData {
                    x: width as f64 / 64.0,
                    y: height as f64 / 64.0,
                    delta_x: 0.0,
                    delta_y: 0.0,
                    unit: ScrollUnit::Pixel,
                    modifiers: EventModifiers::default(),
                }),
            ));
        }
        let turns = if events.is_empty() {
            0
        } else {
            self.measurement_feedback_turns.saturating_add(1)
        };
        if turns > self.config.max_measurement_feedback_turns {
            return Err(NativeUiHostError::MeasurementFeedbackLimitExceeded);
        }
        if self.events.len().saturating_add(events.len()) > self.config.max_pending_events {
            return Err(NativeUiHostError::EventQueueFull);
        }
        let count =
            u64::try_from(events.len()).map_err(|_| NativeUiHostError::EventSequenceExhausted)?;
        self.next_event_sequence
            .checked_add(count)
            .ok_or(NativeUiHostError::EventSequenceExhausted)?;
        Ok(MeasurementFeedback {
            measurements,
            events,
            turns,
        })
    }

    fn publish_measurement_feedback(
        &mut self,
        feedback: MeasurementFeedback,
    ) -> Result<(), NativeUiHostError> {
        self.measurements = feedback.measurements;
        self.measurement_feedback_turns = feedback.turns;
        self.enqueue_events(feedback.events)
    }

    fn normalize_input(
        &mut self,
        input: &NativeInputKind,
    ) -> Option<(NodeId, EventType, EventPayload)> {
        match input {
            NativeInputKind::PointerMoved {
                device,
                x_milli,
                y_milli,
                ..
            } => {
                let (x, y) = points(*x_milli, *y_milli);
                Some((
                    self.captured_pointers
                        .get(device)
                        .copied()
                        .or_else(|| self.hit_test(x, y))?,
                    EventType::POINTER_MOVE,
                    pointer_payload(*device, x, y, -1, 0),
                ))
            }
            NativeInputKind::PointerButton { .. } => None,
            NativeInputKind::Wheel {
                x_milli,
                y_milli,
                delta_x_milli,
                delta_y_milli,
                unit,
                ..
            } => {
                let (x, y) = points(*x_milli, *y_milli);
                Some((
                    self.hit_test(x, y)?,
                    EventType::WHEEL,
                    EventPayload::Scroll(ScrollEventData {
                        x,
                        y,
                        delta_x: f64::from(*delta_x_milli) / 1_000.0,
                        delta_y: f64::from(*delta_y_milli) / 1_000.0,
                        unit: match unit {
                            vo_app_host_native::NativeScrollUnit::Pixel => ScrollUnit::Pixel,
                            vo_app_host_native::NativeScrollUnit::Line => ScrollUnit::Line,
                            vo_app_host_native::NativeScrollUnit::Page => ScrollUnit::Page,
                        },
                        modifiers: EventModifiers::default(),
                    }),
                ))
            }
            NativeInputKind::Key {
                physical_key,
                logical_key,
                pressed,
                repeat,
                modifiers,
                ..
            } => Some((
                self.focused?,
                if *pressed {
                    EventType::KEY_DOWN
                } else {
                    EventType::KEY_UP
                },
                EventPayload::Key(KeyEventData {
                    key: logical_key.clone(),
                    code: physical_key.to_string(),
                    modifiers: event_modifiers(*modifiers),
                    repeat: *repeat,
                    composing: false,
                }),
            )),
            NativeInputKind::Text(text) => {
                let focused = self.focused?;
                let value = edited_control_value(&self.tree, focused, TextEdit::Insert(text))?;
                Some((focused, EventType::INPUT, EventPayload::TextInput(value)))
            }
            NativeInputKind::ImeStarted => Some((
                self.focused?,
                EventType::COMPOSITION_START,
                EventPayload::Composition(CompositionEventData {
                    text: String::new(),
                    selection_start_utf16: 0,
                    selection_length_utf16: 0,
                }),
            )),
            NativeInputKind::ImeUpdated {
                text,
                selection_start_utf16,
                selection_len_utf16,
            } => Some((
                self.focused?,
                EventType::COMPOSITION_UPDATE,
                EventPayload::Composition(CompositionEventData {
                    text: text.clone(),
                    selection_start_utf16: *selection_start_utf16,
                    selection_length_utf16: *selection_len_utf16,
                }),
            )),
            NativeInputKind::ImeCommitted(text) => Some((
                self.focused?,
                EventType::COMPOSITION_END,
                EventPayload::Composition(CompositionEventData {
                    text: text.clone(),
                    selection_start_utf16: 0,
                    selection_length_utf16: 0,
                }),
            )),
            NativeInputKind::ImeCancelled => Some((
                self.focused?,
                EventType::COMPOSITION_END,
                EventPayload::Composition(CompositionEventData {
                    text: String::new(),
                    selection_start_utf16: 0,
                    selection_length_utf16: 0,
                }),
            )),
            NativeInputKind::FocusChanged(focused) => {
                let target = self.focused?;
                Some((
                    target,
                    if *focused {
                        EventType::FOCUS
                    } else {
                        EventType::BLUR
                    },
                    EventPayload::None,
                ))
            }
            NativeInputKind::ModifiersChanged(_)
            | NativeInputKind::GamepadSnapshot { .. }
            | NativeInputKind::DeviceDisconnected { .. }
            | NativeInputKind::FileDragEntered { .. }
            | NativeInputKind::FileDragMoved { .. }
            | NativeInputKind::FileDragLeft
            | NativeInputKind::FileDropped { .. }
            | NativeInputKind::VisibilityChanged(_)
            | NativeInputKind::Resized { .. }
            | NativeInputKind::CloseRequested => None,
        }
    }

    fn keyboard_text_edit(&self, input: &NativeInputKind) -> Option<TextInputEventData> {
        let focused = self.focused?;
        match input {
            NativeInputKind::Key {
                logical_key,
                pressed: true,
                ..
            } if logical_key == "Backspace" => {
                edited_control_value(&self.tree, focused, TextEdit::Backspace)
            }
            NativeInputKind::Key {
                logical_key,
                pressed: true,
                ..
            } if logical_key == "Delete" => {
                edited_control_value(&self.tree, focused, TextEdit::Delete)
            }
            NativeInputKind::ImeCommitted(text) if !text.is_empty() => {
                edited_control_value(&self.tree, focused, TextEdit::Insert(text))
            }
            _ => None,
        }
    }

    fn keyboard_text_selection(&self, input: &NativeInputKind) -> Option<TextInputEventData> {
        let focused = self.focused?;
        let NativeInputKind::Key {
            logical_key,
            pressed: true,
            modifiers,
            ..
        } = input
        else {
            return None;
        };
        if !matches!(
            logical_key.as_str(),
            "ArrowLeft" | "ArrowRight" | "ArrowUp" | "ArrowDown" | "Home" | "End"
        ) {
            return None;
        }
        let snapshot = self.tree.node(focused)?;
        let NodeKind::Element(primitive @ (Primitive::TextInput | Primitive::TextArea)) =
            snapshot.kind
        else {
            return None;
        };
        let Value::Text(value) = snapshot.properties.get(&PropertyId::VALUE)? else {
            return None;
        };
        let controlled = controlled_text_selection(&self.tree, focused)
            .ok()
            .flatten()
            .unwrap_or(NativeTextSelection {
                start_utf16: u32::try_from(value.encode_utf16().count()).ok()?,
                length_utf16: 0,
            });
        let start = controlled.start_utf16;
        let end = start.checked_add(controlled.length_utf16)?;
        let moving_backward = matches!(logical_key.as_str(), "ArrowLeft" | "ArrowUp" | "Home");
        let origin = if moving_backward { start } else { end };
        let moved = move_text_caret(value, primitive, logical_key, origin)?;
        let (selection_start_utf16, selection_length_utf16) = if modifiers.shift {
            let anchor = if moving_backward { end } else { start };
            (
                anchor.min(moved),
                anchor.max(moved).saturating_sub(anchor.min(moved)),
            )
        } else {
            let collapsed = if controlled.length_utf16 > 0
                && matches!(logical_key.as_str(), "ArrowLeft" | "ArrowRight")
            {
                origin
            } else {
                moved
            };
            (collapsed, 0)
        };
        if selection_start_utf16 == controlled.start_utf16
            && selection_length_utf16 == controlled.length_utf16
        {
            return None;
        }
        Some(TextInputEventData {
            text: value.clone(),
            selection_start_utf16,
            selection_length_utf16,
        })
    }

    fn hit_test(&self, x: f64, y: f64) -> Option<NodeId> {
        self.layouts
            .values()
            .filter(|layout| {
                layout.rect.contains(x, y)
                    && layout.clip.is_none_or(|clip| clip.contains(x, y))
                    && self
                        .active_modal
                        .is_none_or(|modal| tree_contains(&self.tree, modal, layout.node))
                    && tree_accepts_pointer_events(&self.tree, layout.node)
            })
            .max_by_key(|layout| (layout.z_index, layout.node))
            .map(|layout| layout.node)
    }

    fn cycle_modal_focus(&mut self, reverse: bool) {
        let Some(modal) = self.active_modal else {
            return;
        };
        if self.modal_focus_order.is_empty() {
            self.focused = Some(modal);
            return;
        }
        let current = self.focused.and_then(|focused| {
            self.modal_focus_order
                .iter()
                .position(|node| *node == focused)
        });
        let next = if reverse {
            current.map_or(self.modal_focus_order.len() - 1, |index| {
                if index == 0 {
                    self.modal_focus_order.len() - 1
                } else {
                    index - 1
                }
            })
        } else {
            current.map_or(0, |index| (index + 1) % self.modal_focus_order.len())
        };
        self.focused = self.modal_focus_order.get(next).copied().or(Some(modal));
    }

    fn listener_target(
        &self,
        mut node: NodeId,
        event: EventType,
    ) -> Option<(NodeId, vo_ui_core::HandlerId)> {
        loop {
            let snapshot = self.tree.node(node)?;
            if let Some(listener) = snapshot.listeners.get(&event) {
                return Some((node, listener.handler));
            }
            node = snapshot.parent?;
        }
    }
}

impl DesktopHost for NativeUiHost {
    type Error = NativeUiHostError;

    fn apply_atomic(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
        let mut staged_tree = self.tree.clone();
        staged_tree
            .apply(batch)
            .map_err(NativeUiHostError::Protocol)?;
        let mut selection_nodes = std::collections::BTreeSet::new();
        for mutation in &batch.mutations {
            match mutation {
                Mutation::SetProperty { id, property }
                    if matches!(
                        property.id,
                        PropertyId::SELECTION_START_UTF16 | PropertyId::SELECTION_LENGTH_UTF16
                    ) =>
                {
                    selection_nodes.insert(*id);
                }
                Mutation::RemoveProperty { id, property }
                    if matches!(
                        *property,
                        PropertyId::SELECTION_START_UTF16 | PropertyId::SELECTION_LENGTH_UTF16
                    ) =>
                {
                    selection_nodes.insert(*id);
                }
                _ => {}
            }
        }
        for node in selection_nodes {
            if staged_tree.node(node).is_some() {
                controlled_text_selection(&staged_tree, node)?;
            }
        }
        let modal = modal_focus_state(&staged_tree, self.config.max_layout_nodes)?;
        let focus_request = focus_request_state(
            &staged_tree,
            &self.tree,
            self.config.max_layout_nodes,
            modal.as_ref().map(|state| state.scope),
        )?;
        let mut staged_focused = self.focused;
        let mut staged_restore = self.focus_restore;
        let mut staged_last_focus_request = self.last_focus_request;
        match (&modal, self.active_modal) {
            (Some(next), Some(current)) if next.scope == current => {
                if staged_focused
                    .is_none_or(|focused| !tree_contains(&staged_tree, current, focused))
                {
                    staged_focused = Some(next.preferred);
                }
            }
            (Some(next), None) => {
                staged_restore = staged_focused;
                staged_focused = Some(next.preferred);
            }
            (Some(next), Some(_)) => {
                staged_focused = Some(next.preferred);
            }
            (None, Some(_)) => {
                staged_focused = staged_restore.filter(|node| staged_tree.node(*node).is_some());
                staged_restore = None;
            }
            (None, None) => {}
        }
        match focus_request {
            Some(request) if Some(request) != staged_last_focus_request => {
                staged_focused = Some(request.0);
                staged_last_focus_request = Some(request);
            }
            Some(_) => {}
            None => staged_last_focus_request = None,
        }
        let mut staged_layouts = self.layouts.clone();
        let mut staged_scroll = self.scroll.clone();
        for mutation in &batch.mutations {
            match mutation {
                Mutation::Delete { id } => {
                    staged_layouts.remove(id);
                    staged_scroll.remove(id);
                    if staged_focused == Some(*id) {
                        staged_focused = modal.as_ref().map(|state| state.preferred);
                    }
                }
                Mutation::SetProperty { id, property }
                    if matches!(property.id, PropertyId::SCROLL_X | PropertyId::SCROLL_Y) =>
                {
                    staged_scroll.remove(id);
                }
                Mutation::RemoveProperty { id, property }
                    if matches!(*property, PropertyId::SCROLL_X | PropertyId::SCROLL_Y) =>
                {
                    staged_scroll.remove(id);
                }
                _ => {}
            }
        }
        if staged_focused.is_some_and(|focused| staged_tree.node(focused).is_none()) {
            staged_focused = modal
                .as_ref()
                .map(|state| state.preferred)
                .filter(|node| staged_tree.node(*node).is_some());
        }
        staged_restore = staged_restore.filter(|node| staged_tree.node(*node).is_some());
        staged_last_focus_request =
            staged_last_focus_request.filter(|(node, _)| staged_tree.node(*node).is_some());
        let mut staged_events = self.events.clone();
        staged_events.retain(|incoming| {
            let event = &incoming.event;
            if event.event == EventType::INVALIDATE
                && event.handler == HandlerId::new(u32::MAX, 1)
                && event.target == staged_tree.root()
                && event.payload == EventPayload::None
            {
                return true;
            }
            staged_tree.node(event.target).is_some_and(|target| {
                target
                    .listeners
                    .get(&event.event)
                    .is_some_and(|listener| listener.handler == event.handler)
            })
        });
        self.tree = staged_tree;
        self.layouts = staged_layouts;
        self.events = staged_events;
        self.focused = staged_focused;
        self.active_modal = modal.as_ref().map(|state| state.scope);
        self.modal_focus_order = modal.map_or_else(Vec::new, |state| state.focus_order);
        self.last_focus_request = staged_last_focus_request;
        self.focus_restore = staged_restore;
        self.scroll = staged_scroll;
        self.pressed_pointers
            .retain(|_, node| self.tree.node(*node).is_some());
        if self
            .hovered
            .is_some_and(|node| self.tree.node(node).is_none())
        {
            self.hovered = None;
        }
        self.captured_pointers
            .retain(|_, node| self.tree.node(*node).is_some());
        Ok(())
    }

    fn poll_event(&mut self) -> Result<Option<EventEnvelope>, Self::Error> {
        Ok(self.events.pop_front())
    }
}

struct ModalFocusState {
    scope: NodeId,
    preferred: NodeId,
    focus_order: Vec<NodeId>,
}

fn boolean_property(
    node: &vo_ui_protocol::NodeSnapshot,
    property: PropertyId,
) -> Result<Option<bool>, NativeUiHostError> {
    match node.properties.get(&property) {
        None => Ok(None),
        Some(Value::Bool(value)) => Ok(Some(*value)),
        Some(_) => Err(NativeUiHostError::InvalidModalProperty(node.id, property)),
    }
}

fn modal_focus_state(
    tree: &TreeMirror,
    max_nodes: usize,
) -> Result<Option<ModalFocusState>, NativeUiHostError> {
    let mut stack = vec![(tree.root(), false, false)];
    let mut visited = 0_usize;
    let mut scope = None;
    while let Some((id, inside_portal, ancestor_hidden)) = stack.pop() {
        visited = visited
            .checked_add(1)
            .ok_or(NativeUiHostError::LayoutLimitExceeded)?;
        if visited > max_nodes {
            return Err(NativeUiHostError::LayoutLimitExceeded);
        }
        let node = tree
            .node(id)
            .ok_or(NativeUiHostError::MissingLayoutNode(id))?;
        let hidden =
            ancestor_hidden || boolean_property(&node, PropertyId::HIDDEN)?.unwrap_or(false);
        let modal = boolean_property(&node, PropertyId::MODAL)?.unwrap_or(false);
        if !hidden && modal && scope.replace(id).is_some() {
            return Err(NativeUiHostError::MultipleModalScopes);
        }
        let _ = boolean_property(&node, PropertyId::AUTO_FOCUS)?;
        if let Some(value) = node.properties.get(&PropertyId::POINTER_EVENTS) {
            if !matches!(value, Value::Text(value) if value == "auto" || value == "none") {
                return Err(NativeUiHostError::InvalidPointerEvents(id));
            }
        }
        if let Some(value) = node.properties.get(&PropertyId::POINTER_CAPTURE) {
            if !matches!(value, Value::Bool(_)) {
                return Err(NativeUiHostError::InvalidPointerCapture(id));
            }
        }
        if let Some(value) = node.properties.get(&PropertyId::FLOW_DIRECTION) {
            if !matches!(value, Value::I64(0) | Value::I64(1)) {
                return Err(NativeUiHostError::InvalidFlowDirection(id));
            }
        }
        if let Some(value) = node.properties.get(&PropertyId::PORTAL_LAYER) {
            if !matches!(value, Value::I64(-1_000_000..=1_000_000)) {
                return Err(NativeUiHostError::InvalidPortalLayer(id));
            }
            if inside_portal {
                return Err(NativeUiHostError::NestedPortal(id));
            }
        }
        let enters_portal =
            inside_portal || node.properties.contains_key(&PropertyId::PORTAL_LAYER);
        stack.extend(
            node.children
                .iter()
                .rev()
                .map(|child| (*child, enters_portal, hidden)),
        );
    }
    let Some(scope) = scope else {
        return Ok(None);
    };

    let mut stack = vec![(scope, false)];
    let mut focus_order = Vec::new();
    let mut preferred = None;
    while let Some((id, ancestor_hidden)) = stack.pop() {
        let node = tree
            .node(id)
            .ok_or(NativeUiHostError::MissingLayoutNode(id))?;
        let hidden =
            ancestor_hidden || boolean_property(&node, PropertyId::HIDDEN)?.unwrap_or(false);
        if hidden {
            stack.extend(node.children.iter().rev().map(|child| (*child, true)));
            continue;
        }
        if boolean_property(&node, PropertyId::AUTO_FOCUS)?.unwrap_or(false)
            && preferred.replace(id).is_some()
        {
            return Err(NativeUiHostError::MultipleAutoFocusTargets(scope));
        }
        let focusable = matches!(
            node.kind,
            NodeKind::Element(
                Primitive::Button
                    | Primitive::TextInput
                    | Primitive::TextArea
                    | Primitive::Toggle
                    | Primitive::Slider
            )
        ) || boolean_property(&node, PropertyId::FOCUSABLE)?.unwrap_or(false);
        let focusable = focusable
            && !matches!(
                node.properties.get(&PropertyId::DISABLED),
                Some(Value::Bool(true))
            );
        if focusable {
            focus_order.push(id);
        }
        stack.extend(node.children.iter().rev().map(|child| (*child, false)));
    }
    let preferred = preferred
        .or_else(|| focus_order.first().copied())
        .unwrap_or(scope);
    Ok(Some(ModalFocusState {
        scope,
        preferred,
        focus_order,
    }))
}

fn focus_request_state(
    tree: &TreeMirror,
    previous: &TreeMirror,
    max_nodes: usize,
    active_modal: Option<NodeId>,
) -> Result<Option<(NodeId, i64)>, NativeUiHostError> {
    let mut stack = vec![(tree.root(), false)];
    let mut visited = 0_usize;
    let mut request = None;
    while let Some((id, ancestor_hidden)) = stack.pop() {
        visited = visited
            .checked_add(1)
            .ok_or(NativeUiHostError::LayoutLimitExceeded)?;
        if visited > max_nodes {
            return Err(NativeUiHostError::LayoutLimitExceeded);
        }
        let node = tree
            .node(id)
            .ok_or(NativeUiHostError::MissingLayoutNode(id))?;
        let hidden =
            ancestor_hidden || boolean_property(&node, PropertyId::HIDDEN)?.unwrap_or(false);
        if let Some(value) = node.properties.get(&PropertyId::FOCUS_REQUEST) {
            let Value::I64(token) = value else {
                return Err(NativeUiHostError::InvalidFocusRequest(id));
            };
            if *token < 0 {
                return Err(NativeUiHostError::InvalidFocusRequest(id));
            }
            if *token > 0 && !hidden {
                let unchanged = matches!(
                    previous
                        .node(id)
                        .and_then(|node| node.properties.get(&PropertyId::FOCUS_REQUEST).cloned()),
                    Some(Value::I64(previous_token)) if previous_token == *token
                );
                if unchanged {
                    stack.extend(node.children.iter().rev().map(|child| (*child, hidden)));
                    continue;
                }
                if request.replace((id, *token)).is_some() {
                    return Err(NativeUiHostError::MultipleFocusRequests);
                }
                if active_modal.is_some_and(|modal| !tree_contains(tree, modal, id)) {
                    return Err(NativeUiHostError::FocusRequestOutsideModal(id));
                }
            }
        }
        stack.extend(node.children.iter().rev().map(|child| (*child, hidden)));
    }
    Ok(request)
}

fn tree_contains(tree: &TreeMirror, ancestor: NodeId, node: NodeId) -> bool {
    if ancestor == node {
        return true;
    }
    let mut current = tree.node(node).and_then(|snapshot| snapshot.parent);
    while let Some(id) = current {
        if id == ancestor {
            return true;
        }
        current = tree.node(id).and_then(|snapshot| snapshot.parent);
    }
    false
}

fn tree_accepts_pointer_events(tree: &TreeMirror, node: NodeId) -> bool {
    let mut current = Some(node);
    while let Some(id) = current {
        let Some(snapshot) = tree.node(id) else {
            return false;
        };
        if let Some(Value::Text(value)) = snapshot.properties.get(&PropertyId::POINTER_EVENTS) {
            return value == "auto";
        }
        current = snapshot.parent;
    }
    true
}

fn captures_pointer(tree: &TreeMirror, node: NodeId) -> bool {
    tree.node(node).and_then(|snapshot| {
        snapshot
            .properties
            .get(&PropertyId::POINTER_CAPTURE)
            .cloned()
    }) == Some(Value::Bool(true))
}

fn controlled_text_selection(
    tree: &TreeMirror,
    node: NodeId,
) -> Result<Option<NativeTextSelection>, NativeUiHostError> {
    let snapshot = tree
        .node(node)
        .ok_or(NativeUiHostError::MissingLayoutNode(node))?;
    let start = snapshot.properties.get(&PropertyId::SELECTION_START_UTF16);
    let length = snapshot.properties.get(&PropertyId::SELECTION_LENGTH_UTF16);
    if start.is_none() && length.is_none() {
        return Ok(None);
    }
    if !matches!(
        snapshot.kind,
        NodeKind::Element(Primitive::TextInput | Primitive::TextArea)
    ) {
        return Err(NativeUiHostError::InvalidTextSelectionTarget(node));
    }
    let value = |property: Option<&Value>| match property {
        None => Ok(0),
        Some(Value::I64(value)) if *value >= 0 => {
            u32::try_from(*value).map_err(|_| NativeUiHostError::InvalidTextSelectionValue(node))
        }
        Some(_) => Err(NativeUiHostError::InvalidTextSelectionValue(node)),
    };
    Ok(Some(NativeTextSelection {
        start_utf16: value(start)?,
        length_utf16: value(length)?,
    }))
}

#[derive(Clone, Copy)]
enum TextEdit<'a> {
    Insert(&'a str),
    Backspace,
    Delete,
}

fn edited_control_value(
    tree: &TreeMirror,
    node: NodeId,
    edit: TextEdit<'_>,
) -> Option<TextInputEventData> {
    let snapshot = tree.node(node)?;
    if !matches!(
        snapshot.kind,
        NodeKind::Element(Primitive::TextInput | Primitive::TextArea)
    ) {
        return None;
    }
    let value = match snapshot.properties.get(&PropertyId::VALUE) {
        Some(Value::Text(value)) => value,
        _ => return None,
    };
    let units = value.encode_utf16().count();
    let selection = controlled_text_selection(tree, node).ok().flatten();
    let start_utf16 = selection
        .map(|selection| selection.start_utf16 as usize)
        .unwrap_or(units)
        .min(units);
    let end_utf16 = selection
        .map(|selection| selection.start_utf16.saturating_add(selection.length_utf16) as usize)
        .unwrap_or(start_utf16)
        .min(units);
    let mut start = utf16_byte_offset(value, start_utf16)?;
    let mut end = utf16_byte_offset(value, end_utf16)?;
    match edit {
        TextEdit::Insert(_) => {}
        TextEdit::Backspace if start == end => {
            start = value[..start]
                .char_indices()
                .next_back()
                .map(|(index, _)| index)
                .unwrap_or(start);
        }
        TextEdit::Delete if start == end => {
            end += value[end..].chars().next().map(char::len_utf8).unwrap_or(0);
        }
        TextEdit::Backspace | TextEdit::Delete => {}
    }
    let replacement = match edit {
        TextEdit::Insert(text) => text,
        TextEdit::Backspace | TextEdit::Delete => "",
    };
    let mut updated = String::with_capacity(value.len() - (end - start) + replacement.len());
    updated.push_str(&value[..start]);
    updated.push_str(replacement);
    updated.push_str(&value[end..]);
    let caret = value[..start]
        .encode_utf16()
        .count()
        .checked_add(replacement.encode_utf16().count())?;
    Some(TextInputEventData {
        text: updated,
        selection_start_utf16: u32::try_from(caret).ok()?,
        selection_length_utf16: 0,
    })
}

fn move_text_caret(
    value: &str,
    primitive: Primitive,
    key: &str,
    current_utf16: u32,
) -> Option<u32> {
    let current = usize::try_from(current_utf16).ok()?;
    let current_byte = utf16_byte_offset(value, current)?;
    let byte = match key {
        "ArrowLeft" => value[..current_byte]
            .char_indices()
            .next_back()
            .map_or(current_byte, |(index, _)| index),
        "ArrowRight" => {
            current_byte
                + value[current_byte..]
                    .chars()
                    .next()
                    .map(char::len_utf8)
                    .unwrap_or(0)
        }
        "Home" if primitive == Primitive::TextArea => value[..current_byte]
            .rfind('\n')
            .map_or(0, |index| index + 1),
        "End" if primitive == Primitive::TextArea => value[current_byte..]
            .find('\n')
            .map_or(value.len(), |index| current_byte + index),
        "Home" => 0,
        "End" => value.len(),
        "ArrowUp" if primitive == Primitive::TextArea => {
            let line_start = value[..current_byte]
                .rfind('\n')
                .map_or(0, |index| index + 1);
            if line_start == 0 {
                0
            } else {
                let previous_end = line_start - 1;
                let previous_start = value[..previous_end]
                    .rfind('\n')
                    .map_or(0, |index| index + 1);
                text_column_byte(
                    value,
                    previous_start,
                    previous_end,
                    value[line_start..current_byte].chars().count(),
                )
            }
        }
        "ArrowDown" if primitive == Primitive::TextArea => {
            let line_start = value[..current_byte]
                .rfind('\n')
                .map_or(0, |index| index + 1);
            let Some(line_end_relative) = value[current_byte..].find('\n') else {
                return u32::try_from(value.encode_utf16().count()).ok();
            };
            let next_start = current_byte + line_end_relative + 1;
            let next_end = value[next_start..]
                .find('\n')
                .map_or(value.len(), |index| next_start + index);
            text_column_byte(
                value,
                next_start,
                next_end,
                value[line_start..current_byte].chars().count(),
            )
        }
        "ArrowUp" => 0,
        "ArrowDown" => value.len(),
        _ => return None,
    };
    u32::try_from(value[..byte].encode_utf16().count()).ok()
}

fn text_column_byte(value: &str, start: usize, end: usize, column: usize) -> usize {
    value[start..end]
        .char_indices()
        .nth(column)
        .map_or(end, |(offset, _)| start + offset)
}

fn utf16_byte_offset(value: &str, offset: usize) -> Option<usize> {
    if offset == 0 {
        return Some(0);
    }
    let mut units = 0_usize;
    for (index, character) in value.char_indices() {
        if units == offset {
            return Some(index);
        }
        units += character.len_utf16();
        if units > offset {
            return None;
        }
    }
    (units == offset).then_some(value.len())
}

fn quantized_measurement(value: f64) -> Option<i64> {
    let scaled = value * 64.0;
    (value.is_finite() && value >= 0.0 && scaled <= i64::MAX as f64).then(|| scaled.round() as i64)
}

fn points(x_milli: i32, y_milli: i32) -> (f64, f64) {
    (f64::from(x_milli) / 1_000.0, f64::from(y_milli) / 1_000.0)
}

fn sync_scroll_metrics(states: &mut BTreeMap<NodeId, NativeScrollState>, layout: &LayoutSnapshot) {
    let live = layout
        .scroll_iter()
        .map(|metrics| metrics.node)
        .collect::<std::collections::BTreeSet<_>>();
    states.retain(|node, _| live.contains(node));
    for metrics in layout.scroll_iter() {
        let state = states
            .entry(metrics.node)
            .or_insert_with(|| NativeScrollState {
                offset_x: metrics.offset_x.clamp(0.0, metrics.max_offset_x),
                offset_y: metrics.offset_y.clamp(0.0, metrics.max_offset_y),
                ..NativeScrollState::default()
            });
        state.max_offset_x = metrics.max_offset_x;
        state.max_offset_y = metrics.max_offset_y;
        state.viewport_width = metrics.viewport.width;
        state.viewport_height = metrics.viewport.height;
        state.offset_x = state.offset_x.clamp(0.0, state.max_offset_x);
        state.offset_y = state.offset_y.clamp(0.0, state.max_offset_y);
    }
}

fn apply_scroll_impulse(
    offset: &mut f64,
    overscroll: &mut f64,
    velocity: &mut f64,
    max_offset: f64,
    delta: f64,
    overscroll_limit: f64,
) -> bool {
    if delta == 0.0 || (!delta.is_finite()) {
        return false;
    }
    let previous = (*offset, *overscroll);
    let desired = *offset + delta;
    if desired < 0.0 {
        *offset = 0.0;
        *overscroll = (*overscroll + desired * 0.35).clamp(-overscroll_limit, overscroll_limit);
    } else if desired > max_offset {
        *offset = max_offset;
        *overscroll = (*overscroll + (desired - max_offset) * 0.35)
            .clamp(-overscroll_limit, overscroll_limit);
    } else {
        *offset = desired;
        *overscroll *= 0.5;
    }
    *velocity = delta * 20.0;
    previous != (*offset, *overscroll)
}

#[allow(clippy::too_many_arguments)]
fn advance_scroll_axis(
    offset: &mut f64,
    overscroll: &mut f64,
    velocity: &mut f64,
    max_offset: f64,
    seconds: f64,
    momentum: f64,
    recovery: f64,
) {
    if velocity.abs() >= 0.1 {
        let desired = *offset + *velocity * seconds;
        if desired < 0.0 {
            *overscroll += desired * 0.25;
            *offset = 0.0;
            *velocity = 0.0;
        } else if desired > max_offset {
            *overscroll += (desired - max_offset) * 0.25;
            *offset = max_offset;
            *velocity = 0.0;
        } else {
            *offset = desired;
            *velocity *= momentum;
        }
    }
    *overscroll *= recovery;
    if velocity.abs() < 0.1 {
        *velocity = 0.0;
    }
    if overscroll.abs() < 0.1 {
        *overscroll = 0.0;
    }
}

fn pointer_payload(device: u64, x: f64, y: f64, button: i16, buttons: u16) -> EventPayload {
    EventPayload::Pointer(PointerEventData {
        x,
        y,
        button,
        buttons,
        pointer_id: i64::try_from(device).unwrap_or(i64::MAX),
        kind: PointerKind::Mouse,
        modifiers: EventModifiers::default(),
    })
}

fn pointer_button(button: NativePointerButton) -> i16 {
    match button {
        NativePointerButton::Primary => 0,
        NativePointerButton::Secondary => 2,
        NativePointerButton::Middle => 1,
        NativePointerButton::Auxiliary(value) => i16::try_from(value).unwrap_or(i16::MAX),
    }
}

fn event_modifiers(modifiers: NativeModifiers) -> EventModifiers {
    EventModifiers {
        shift: modifiers.shift,
        control: modifiers.control,
        alt: modifiers.alt,
        meta: modifiers.meta,
    }
}

fn keyboard_activation(input: &NativeInputKind) -> bool {
    let NativeInputKind::Key {
        logical_key,
        pressed,
        repeat,
        ..
    } = input
    else {
        return false;
    };
    !repeat
        && ((logical_key == "Enter" && *pressed)
            || ((logical_key == " " || logical_key == "Space") && !pressed))
}

fn keyboard_activatable(tree: &TreeMirror, node: NodeId) -> bool {
    let Some(node) = tree.node(node) else {
        return false;
    };
    if node.properties.get(&PropertyId::DISABLED) == Some(&Value::Bool(true)) {
        return false;
    }
    matches!(
        node.kind,
        NodeKind::Element(Primitive::Button | Primitive::Toggle)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_app_protocol::GenerationalHandle;
    use vo_ui_core::{HandlerId, Listener, Primitive, Property};
    use vo_ui_desktop::DesktopRenderer;
    use vo_ui_layout::ApproximateTextMeasurer;
    use vo_ui_paint::DrawCommand;
    use vo_ui_protocol::{NodeKind, Renderer};

    fn handles() -> (WindowHandle, ViewHandle) {
        (
            GenerationalHandle {
                index: 1,
                generation: 1,
            },
            GenerationalHandle {
                index: 2,
                generation: 1,
            },
        )
    }

    fn renderer() -> DesktopRenderer<NativeUiHost> {
        let (window, view) = handles();
        let root = NodeId::new(0, 1);
        let host = NativeUiHost::new(
            window,
            view,
            7,
            root,
            ProtocolLimits::default(),
            NativeUiHostConfig::default(),
        )
        .unwrap();
        let mut renderer = DesktopRenderer::new(host, 7, root, ProtocolLimits::default());
        let input = NodeId::new(1, 1);
        renderer
            .apply(&MutationBatch::new(
                7,
                1,
                vec![
                    Mutation::Create {
                        id: input,
                        kind: NodeKind::Element(Primitive::TextInput),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::POINTER_DOWN, HandlerId::new(1, 1)),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::KEY_DOWN, HandlerId::new(2, 1)),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(
                            EventType::COMPOSITION_UPDATE,
                            HandlerId::new(3, 1),
                        ),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::CLICK, HandlerId::new(4, 1)),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::INPUT, HandlerId::new(5, 1)),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::SELECTION_CHANGE, HandlerId::new(6, 1)),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::CONTEXT_MENU, HandlerId::new(7, 1)),
                    },
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(PropertyId::VALUE, "a😀bc"),
                    },
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(PropertyId::SELECTION_START_UTF16, 1_i64),
                    },
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(PropertyId::SELECTION_LENGTH_UTF16, 2_i64),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: input,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        renderer
            .host_mut()
            .compute_and_set_layout(
                Size::new(300.0, 200.0),
                LayoutLimits::default(),
                &mut ApproximateTextMeasurer,
            )
            .unwrap();
        renderer
    }

    fn event(kind: NativeInputKind) -> NativeInputEvent {
        let (window, view) = handles();
        NativeInputEvent {
            sequence: 1,
            timestamp_micros: 1,
            window,
            view,
            kind,
        }
    }

    #[test]
    fn semantic_toggle_activation_emits_the_next_checked_value() {
        let (window, view) = handles();
        let root = NodeId::new(0, 1);
        let toggle = NodeId::new(2, 1);
        let handler = HandlerId::new(12, 1);
        let mut host = NativeUiHost::new(
            window,
            view,
            7,
            root,
            ProtocolLimits::default(),
            NativeUiHostConfig::default(),
        )
        .unwrap();
        host.apply_atomic(&MutationBatch::new(
            7,
            1,
            vec![
                Mutation::Create {
                    id: toggle,
                    kind: NodeKind::Element(Primitive::Toggle),
                },
                Mutation::Listen {
                    id: toggle,
                    listener: Listener::new(EventType::CHANGE, handler),
                },
                Mutation::SetProperty {
                    id: toggle,
                    property: Property::new(PropertyId::CHECKED, false),
                },
                Mutation::SetProperty {
                    id: toggle,
                    property: Property::new(PropertyId::ACCESSIBLE_NAME, "Work offline"),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: toggle,
                    before: None,
                },
            ],
        ))
        .unwrap();

        assert!(host.route_semantic_invoke(toggle).unwrap());
        let changed = host.events.pop_front().unwrap().event;
        assert_eq!(changed.handler, handler);
        assert_eq!(changed.event, EventType::CHANGE);
        assert_eq!(changed.payload, EventPayload::Toggle(true));

        host.apply_atomic(&MutationBatch::new(
            7,
            2,
            vec![Mutation::SetProperty {
                id: toggle,
                property: Property::new(PropertyId::CHECKED, true),
            }],
        ))
        .unwrap();
        assert!(host.route_semantic_invoke(toggle).unwrap());
        assert_eq!(
            host.events.pop_front().unwrap().event.payload,
            EventPayload::Toggle(false)
        );
    }

    #[test]
    fn pointer_hit_focuses_node_and_key_uses_same_listener_identity() {
        let mut renderer = renderer();
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap());
        let pointer = renderer.poll_event().unwrap().unwrap();
        assert_eq!(pointer.event, EventType::POINTER_DOWN);
        assert_eq!(renderer.host().focused_node(), Some(NodeId::new(1, 1)));

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 5,
                physical_key: 36,
                logical_key: "Enter".to_string(),
                pressed: true,
                repeat: false,
                modifiers: NativeModifiers {
                    shift: true,
                    ..NativeModifiers::default()
                },
            }))
            .unwrap());
        let key = renderer.poll_event().unwrap().unwrap();
        assert_eq!(key.event, EventType::KEY_DOWN);
        assert!(matches!(
            key.payload,
            EventPayload::Key(KeyEventData {
                key,
                modifiers: EventModifiers { shift: true, .. },
                ..
            }) if key == "Enter"
        ));
    }

    #[test]
    fn pointer_and_focus_state_feed_the_native_paint_scene() {
        let mut renderer = renderer();
        let input = NodeId::new(1, 1);
        renderer
            .apply(&MutationBatch::new(
                7,
                2,
                vec![
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(PropertyId::BACKGROUND, Value::Color(0xff112233)),
                    },
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(
                            PropertyId::HOVER_BACKGROUND,
                            Value::Color(0xff223344),
                        ),
                    },
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(
                            PropertyId::PRESSED_BACKGROUND,
                            Value::Color(0xff334455),
                        ),
                    },
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(PropertyId::FOCUS_RING, Value::Color(0xff445566)),
                    },
                ],
            ))
            .unwrap();
        let layout = renderer
            .host_mut()
            .compute_and_set_layout(
                Size::new(300.0, 200.0),
                LayoutLimits::default(),
                &mut ApproximateTextMeasurer,
            )
            .unwrap();
        assert!(!renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerMoved {
                device: 4,
                x_milli: 20_000,
                y_milli: 20_000,
                delta_x_milli: 0,
                delta_y_milli: 0,
                pressure_milli: 0,
            }))
            .unwrap());
        let hovered = renderer
            .host()
            .build_paint_scene(&layout, PaintLimits::default())
            .unwrap();
        assert!(hovered.commands().iter().any(|command| matches!(
            command,
            DrawCommand::FillRect {
                node,
                color: 0xff223344,
                ..
            } if *node == input
        )));

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap());
        let pressed = renderer
            .host()
            .build_paint_scene(&layout, PaintLimits::default())
            .unwrap();
        assert!(pressed.commands().iter().any(|command| matches!(
            command,
            DrawCommand::FillRect {
                node,
                color: 0xff334455,
                ..
            } if *node == input
        )));
        assert!(pressed.commands().iter().any(|command| matches!(
            command,
            DrawCommand::StrokeRect {
                node,
                color: 0xff445566,
                width: 3.0,
                ..
            } if *node == input
        )));
    }

    #[test]
    fn secondary_press_emits_pointer_and_context_menu_with_native_coordinates() {
        let mut renderer = renderer();
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Secondary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 30_000,
            }))
            .unwrap());
        let pointer = renderer.poll_event().unwrap().unwrap();
        assert_eq!(pointer.event, EventType::POINTER_DOWN);
        let context = renderer.poll_event().unwrap().unwrap();
        assert_eq!(context.event, EventType::CONTEXT_MENU);
        assert_eq!(context.handler, HandlerId::new(7, 1));
        assert_eq!(context.target, NodeId::new(1, 1));
        assert!(matches!(
            context.payload,
            EventPayload::Pointer(PointerEventData {
                x: 20.0,
                y: 30.0,
                button: 2,
                buttons: 0,
                pointer_id: 4,
                kind: PointerKind::Mouse,
                ..
            })
        ));
        assert_eq!(renderer.poll_event().unwrap(), None);
    }

    #[test]
    fn committed_revision_discards_queued_events_for_replaced_listeners() {
        let mut renderer = renderer();
        let input = NodeId::new(1, 1);
        let stale = HandlerId::new(1, 1);
        renderer
            .host_mut()
            .enqueue_event(stale, EventType::POINTER_DOWN, input, EventPayload::None)
            .unwrap();
        renderer.host_mut().queue_invalidation().unwrap();

        renderer
            .apply(&MutationBatch::new(
                7,
                2,
                vec![
                    Mutation::Unlisten {
                        id: input,
                        event: EventType::POINTER_DOWN,
                        handler: stale,
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::POINTER_DOWN, HandlerId::new(11, 1)),
                    },
                ],
            ))
            .unwrap();

        let event = renderer.poll_event().unwrap().unwrap();
        assert_eq!(event.event, EventType::INVALIDATE);
        assert!(renderer.poll_event().unwrap().is_none());
    }

    #[test]
    fn enter_and_space_activate_a_focused_button_exactly_once() {
        let mut renderer = renderer();
        let root = NodeId::new(0, 1);
        let button = NodeId::new(2, 1);
        renderer
            .apply(&MutationBatch::new(
                7,
                2,
                vec![
                    Mutation::Create {
                        id: button,
                        kind: NodeKind::Element(Primitive::Button),
                    },
                    Mutation::Listen {
                        id: button,
                        listener: Listener::new(EventType::KEY_DOWN, HandlerId::new(6, 1)),
                    },
                    Mutation::Listen {
                        id: button,
                        listener: Listener::new(EventType::CLICK, HandlerId::new(7, 1)),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: button,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        renderer.host_mut().focused = Some(button);

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 5,
                physical_key: 36,
                logical_key: "Enter".to_string(),
                pressed: true,
                repeat: false,
                modifiers: NativeModifiers::default(),
            }))
            .unwrap());
        assert_eq!(
            renderer.poll_event().unwrap().unwrap().event,
            EventType::KEY_DOWN
        );
        assert_eq!(
            renderer.poll_event().unwrap().unwrap().event,
            EventType::CLICK
        );
        assert!(renderer.poll_event().unwrap().is_none());

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 5,
                physical_key: 49,
                logical_key: " ".to_string(),
                pressed: false,
                repeat: false,
                modifiers: NativeModifiers::default(),
            }))
            .unwrap());
        assert_eq!(
            renderer.poll_event().unwrap().unwrap().event,
            EventType::CLICK
        );
        assert!(renderer.poll_event().unwrap().is_none());
    }

    #[test]
    fn slider_routes_keyboard_and_captured_pointer_values() {
        let mut renderer = renderer();
        let root = NodeId::new(0, 1);
        let slider = NodeId::new(2, 1);
        renderer
            .apply(&MutationBatch::new(
                7,
                2,
                vec![
                    Mutation::Create {
                        id: slider,
                        kind: NodeKind::Element(Primitive::Slider),
                    },
                    Mutation::SetProperty {
                        id: slider,
                        property: Property::new(PropertyId::VALUE, 50.0_f64),
                    },
                    Mutation::SetProperty {
                        id: slider,
                        property: Property::new(PropertyId::MIN_VALUE, 0.0_f64),
                    },
                    Mutation::SetProperty {
                        id: slider,
                        property: Property::new(PropertyId::MAX_VALUE, 100.0_f64),
                    },
                    Mutation::SetProperty {
                        id: slider,
                        property: Property::new(PropertyId::STEP_VALUE, 5.0_f64),
                    },
                    Mutation::Listen {
                        id: slider,
                        listener: Listener::new(EventType::INPUT, HandlerId::new(20, 1)),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: slider,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        renderer
            .host_mut()
            .compute_and_set_layout(
                Size::new(300.0, 200.0),
                LayoutLimits::default(),
                &mut ApproximateTextMeasurer,
            )
            .unwrap();
        renderer.host_mut().focused = Some(slider);
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 5,
                physical_key: 79,
                logical_key: "ArrowRight".to_string(),
                pressed: true,
                repeat: false,
                modifiers: NativeModifiers::default(),
            }))
            .unwrap());
        let changed = renderer.poll_event().unwrap().unwrap();
        assert_eq!(changed.event, EventType::INPUT);
        assert_eq!(changed.payload, EventPayload::Text("55".to_string()));

        let rect = renderer.host().layouts[&slider].rect;
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 9,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: ((rect.x + rect.width * 0.75) * 1_000.0) as i32,
                y_milli: ((rect.y + rect.height * 0.5) * 1_000.0) as i32,
            }))
            .unwrap());
        let changed = renderer.poll_event().unwrap().unwrap();
        assert_eq!(changed.event, EventType::INPUT);
        assert_eq!(changed.payload, EventPayload::Text("75".to_string()));
        assert_eq!(renderer.host().captured_pointers.get(&9), Some(&slider));
    }

    #[test]
    fn pointer_capture_routes_outside_moves_and_focus_loss_cancellation() {
        let mut renderer = renderer();
        let input = NodeId::new(1, 1);
        renderer
            .apply(&MutationBatch::new(
                7,
                2,
                vec![
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::POINTER_MOVE, HandlerId::new(6, 1)),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::POINTER_UP, HandlerId::new(7, 1)),
                    },
                    Mutation::Listen {
                        id: input,
                        listener: Listener::new(EventType::POINTER_CANCEL, HandlerId::new(8, 1)),
                    },
                    Mutation::SetProperty {
                        id: input,
                        property: Property::new(PropertyId::POINTER_CAPTURE, true),
                    },
                ],
            ))
            .unwrap();
        renderer
            .host_mut()
            .compute_and_set_layout(
                Size::new(300.0, 200.0),
                LayoutLimits::default(),
                &mut ApproximateTextMeasurer,
            )
            .unwrap();
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap());
        assert_eq!(
            renderer.poll_event().unwrap().unwrap().event,
            EventType::POINTER_DOWN
        );
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerMoved {
                device: 4,
                x_milli: 900_000,
                y_milli: 700_000,
                delta_x_milli: 880_000,
                delta_y_milli: 680_000,
                pressure_milli: 0,
            }))
            .unwrap());
        let moved = renderer.poll_event().unwrap().unwrap();
        assert_eq!(moved.event, EventType::POINTER_MOVE);
        assert_eq!(moved.target, NodeId::new(1, 1));

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::FocusChanged(false)))
            .unwrap());
        let cancelled = renderer.poll_event().unwrap().unwrap();
        assert_eq!(cancelled.event, EventType::POINTER_CANCEL);
        assert!(matches!(
            cancelled.payload,
            EventPayload::Pointer(PointerEventData { pointer_id: 4, .. })
        ));
    }

    #[test]
    fn primary_press_and_release_synthesizes_one_monotonic_click() {
        let mut renderer = renderer();
        let press = NativeInputKind::PointerButton {
            device: 4,
            button: NativePointerButton::Primary,
            pressed: true,
            click_count: 1,
            x_milli: 20_000,
            y_milli: 20_000,
        };
        assert!(renderer.host_mut().route_input(&event(press)).unwrap());
        let down = renderer.poll_event().unwrap().unwrap();
        assert_eq!(down.event, EventType::POINTER_DOWN);
        assert_eq!(down.sequence, 1);

        let release = NativeInputKind::PointerButton {
            device: 4,
            button: NativePointerButton::Primary,
            pressed: false,
            click_count: 1,
            x_milli: 20_000,
            y_milli: 20_000,
        };
        assert!(renderer.host_mut().route_input(&event(release)).unwrap());
        let click = renderer.poll_event().unwrap().unwrap();
        assert_eq!(click.event, EventType::CLICK);
        assert_eq!(click.handler, HandlerId::new(4, 1));
        assert_eq!(click.sequence, 2);
        assert_eq!(renderer.poll_event().unwrap(), None);
    }

    #[test]
    fn ime_update_routes_through_the_focused_text_input() {
        let mut renderer = renderer();
        renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap();
        let _ = renderer.poll_event().unwrap();
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::ImeUpdated {
                text: "拼音".to_string(),
                selection_start_utf16: 1,
                selection_len_utf16: 1,
            }))
            .unwrap());
        let ime = renderer.poll_event().unwrap().unwrap();
        assert_eq!(ime.event, EventType::COMPOSITION_UPDATE);
        assert_eq!(
            ime.payload,
            EventPayload::Composition(CompositionEventData {
                text: "拼音".to_string(),
                selection_start_utf16: 1,
                selection_length_utf16: 1,
            })
        );
    }

    #[test]
    fn native_host_exposes_and_validates_controlled_text_selection() {
        let mut renderer = renderer();
        assert_eq!(
            renderer
                .host()
                .controlled_text_selection(NodeId::new(1, 1))
                .unwrap(),
            Some(NativeTextSelection {
                start_utf16: 1,
                length_utf16: 2,
            })
        );

        let revision = renderer.host().revision();
        let result = renderer.apply(&MutationBatch::new(
            7,
            revision + 1,
            vec![Mutation::SetProperty {
                id: NodeId::new(1, 1),
                property: Property::new(PropertyId::SELECTION_LENGTH_UTF16, -1_i64),
            }],
        ));
        assert!(matches!(
            result,
            Err(vo_ui_desktop::DesktopRendererError::Host(
                NativeUiHostError::InvalidTextSelectionValue(node)
            )) if node == NodeId::new(1, 1)
        ));
        assert_eq!(renderer.host().revision(), revision);
    }

    #[test]
    fn native_controlled_input_replaces_utf16_selection_with_full_value() {
        let mut renderer = renderer();
        renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap();
        let _ = renderer.poll_event().unwrap();
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Text("X".to_string())))
            .unwrap());
        let input = renderer.poll_event().unwrap().unwrap();
        assert_eq!(input.event, EventType::INPUT);
        assert_eq!(
            input.payload,
            EventPayload::TextInput(TextInputEventData {
                text: "aXbc".to_string(),
                selection_start_utf16: 2,
                selection_length_utf16: 0,
            })
        );
    }

    #[test]
    fn native_arrow_keys_emit_controlled_selection_without_editing_text() {
        let mut renderer = renderer();
        renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap();
        let _ = renderer.poll_event().unwrap();
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 4,
                physical_key: 0,
                logical_key: "ArrowRight".to_string(),
                pressed: true,
                repeat: false,
                modifiers: NativeModifiers::default(),
            }))
            .unwrap());
        let key = renderer.poll_event().unwrap().unwrap();
        assert_eq!(key.event, EventType::KEY_DOWN);
        let selection = renderer.poll_event().unwrap().unwrap();
        assert_eq!(selection.event, EventType::SELECTION_CHANGE);
        assert_eq!(
            selection.payload,
            EventPayload::TextInput(TextInputEventData {
                text: "a😀bc".to_string(),
                selection_start_utf16: 3,
                selection_length_utf16: 0,
            })
        );
    }

    #[test]
    fn rejected_layout_snapshot_preserves_previous_hit_test() {
        let mut renderer = renderer();
        assert_eq!(
            renderer.host_mut().set_layout_snapshot(
                1,
                [NativeUiLayout {
                    node: NodeId::new(99, 1),
                    rect: NativeUiRect::new(0.0, 0.0, 10.0, 10.0).unwrap(),
                    clip: None,
                    z_index: 2,
                }]
            ),
            Err(NativeUiHostError::MissingLayoutNode(NodeId::new(99, 1)))
        );
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap());
    }

    #[test]
    fn native_hit_testing_respects_ancestor_clip() {
        let mut renderer = renderer();
        renderer
            .host_mut()
            .set_layout_snapshot(
                1,
                [NativeUiLayout {
                    node: NodeId::new(1, 1),
                    rect: NativeUiRect::new(0.0, 0.0, 100.0, 100.0).unwrap(),
                    clip: Some(NativeUiRect::new(0.0, 0.0, 10.0, 10.0).unwrap()),
                    z_index: 2,
                }],
            )
            .unwrap();
        assert!(!renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 5_000,
            }))
            .unwrap());
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 4,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 5_000,
                y_milli: 5_000,
            }))
            .unwrap());
    }

    #[test]
    fn native_host_builds_same_revision_accessibility_snapshot() {
        let mut renderer = renderer();
        let layout = renderer
            .host_mut()
            .compute_and_set_layout(
                Size::new(300.0, 200.0),
                LayoutLimits::default(),
                &mut ApproximateTextMeasurer,
            )
            .unwrap();
        let semantics = renderer
            .host()
            .build_accessibility_tree(&layout, AccessibilityLimits::default())
            .unwrap();
        assert_eq!(semantics.revision, renderer.host().revision());
        let input = semantics.get(NodeId::new(1, 1)).unwrap();
        assert_eq!(input.role, vo_ui_accessibility::AccessibilityRole::TextBox);
        assert!(input.actions.focus);
        assert!(input.actions.set_value);
    }

    #[test]
    fn native_frame_preparation_is_revision_bound_and_atomic() {
        let mut renderer = renderer();
        let mut text = NativeTextSystem::default();
        let frame = renderer
            .host_mut()
            .prepare_frame(
                Size::new(300.0, 200.0),
                2.0,
                LayoutLimits::default(),
                PaintLimits::default(),
                &mut text,
            )
            .unwrap();
        assert_eq!(frame.layout.revision, renderer.host().revision());
        assert_eq!(frame.scene.revision, renderer.host().revision());
        assert_eq!(frame.presentation.revision, renderer.host().revision());
        assert_eq!(frame.accessibility.revision, renderer.host().revision());
        assert!(frame.accesskit.tree.is_some());
        assert!(frame.accesskit_full.tree.is_some());
        assert_eq!(frame.presentation.scale(), 2.0);
        assert_eq!(
            (
                frame.presentation.surface.width(),
                frame.presentation.surface.height()
            ),
            (600, 400)
        );

        assert!(matches!(
            renderer.host_mut().prepare_frame(
                Size::new(1.0, 1.0),
                0.0,
                LayoutLimits::default(),
                PaintLimits::default(),
                &mut text,
            ),
            Err(NativeUiPresentationError::Text(
                NativeTextError::InvalidScale
            ))
        ));
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 8,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 20_000,
                y_milli: 20_000,
            }))
            .unwrap());
    }

    #[test]
    fn accesskit_set_value_action_returns_through_the_validated_event_queue() {
        let mut renderer = renderer();
        let input = NodeId::new(1, 1);
        let mut text = NativeTextSystem::default();
        renderer
            .host_mut()
            .prepare_frame(
                Size::new(300.0, 200.0),
                1.0,
                LayoutLimits::default(),
                PaintLimits::default(),
                &mut text,
            )
            .unwrap();
        assert!(renderer
            .host_mut()
            .route_accessibility_action(&ActionRequest {
                action: accesskit::Action::SetValue,
                target_tree: accesskit::TreeId::ROOT,
                target_node: vo_ui_accesskit::access_node_id(input),
                data: Some(accesskit::ActionData::Value(
                    String::from("Ada").into_boxed_str()
                )),
            })
            .unwrap());
        let event = renderer.poll_event().unwrap().unwrap();
        assert_eq!(event.handler, HandlerId::new(5, 1));
        assert_eq!(event.event, EventType::INPUT);
        assert_eq!(event.payload, EventPayload::Text(String::from("Ada")));
    }

    #[test]
    fn native_scroll_wheel_drives_momentum_overscroll_and_scrollbar_frames() {
        let (window, view) = handles();
        let root = NodeId::new(0, 1);
        let scroll = NodeId::new(1, 1);
        let child = NodeId::new(2, 1);
        let host = NativeUiHost::new(
            window,
            view,
            11,
            root,
            ProtocolLimits::default(),
            NativeUiHostConfig::default(),
        )
        .unwrap();
        let mut renderer = DesktopRenderer::new(host, 11, root, ProtocolLimits::default());
        renderer
            .apply(&MutationBatch::new(
                11,
                1,
                vec![
                    Mutation::Create {
                        id: scroll,
                        kind: NodeKind::Element(Primitive::Scroll),
                    },
                    Mutation::SetProperty {
                        id: scroll,
                        property: Property::new(PropertyId::WIDTH, 100_i64),
                    },
                    Mutation::SetProperty {
                        id: scroll,
                        property: Property::new(PropertyId::HEIGHT, 50_i64),
                    },
                    Mutation::Create {
                        id: child,
                        kind: NodeKind::Element(Primitive::Box),
                    },
                    Mutation::SetProperty {
                        id: child,
                        property: Property::new(PropertyId::HEIGHT, 200_i64),
                    },
                    Mutation::InsertBefore {
                        parent: scroll,
                        child,
                        before: None,
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: scroll,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        let mut text = NativeTextSystem::default();
        let first = renderer
            .host_mut()
            .prepare_frame(
                Size::new(200.0, 100.0),
                1.0,
                LayoutLimits::default(),
                PaintLimits::default(),
                &mut text,
            )
            .unwrap();
        assert_eq!(first.presentation.frame_id, 1);
        assert_eq!(
            renderer
                .host()
                .scroll_snapshot(scroll)
                .unwrap()
                .max_offset_y,
            150.0
        );

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Wheel {
                device: 1,
                x_milli: 10_000,
                y_milli: 10_000,
                delta_x_milli: 0,
                delta_y_milli: 10_000,
                unit: vo_app_host_native::NativeScrollUnit::Pixel,
            }))
            .unwrap());
        let after_wheel = renderer.host().scroll_snapshot(scroll).unwrap();
        assert_eq!(after_wheel.offset_y, 10.0);
        assert!(after_wheel.velocity_y > 0.0);
        assert!(renderer
            .host_mut()
            .advance_scroll_physics(Duration::from_millis(16)));

        let second = renderer
            .host_mut()
            .prepare_frame(
                Size::new(200.0, 100.0),
                1.0,
                LayoutLimits::default(),
                PaintLimits::default(),
                &mut text,
            )
            .unwrap();
        assert_eq!(second.presentation.revision, 1);
        assert_eq!(second.presentation.frame_id, 2);
        assert!(second.layout.get(child).unwrap().rect.y < -10.0);
        assert!(second
            .scene
            .commands()
            .iter()
            .any(|command| matches!(command, vo_ui_paint::DrawCommand::Scrollbar { node, .. } if *node == scroll)));

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Wheel {
                device: 1,
                x_milli: 10_000,
                y_milli: 10_000,
                delta_x_milli: 0,
                delta_y_milli: 500_000,
                unit: vo_app_host_native::NativeScrollUnit::Pixel,
            }))
            .unwrap());
        let overscrolled = renderer.host().scroll_snapshot(scroll).unwrap();
        assert_eq!(overscrolled.offset_y, overscrolled.max_offset_y);
        assert!(overscrolled.overscroll_y > 0.0);
        for _ in 0..120 {
            renderer
                .host_mut()
                .advance_scroll_physics(Duration::from_millis(16));
        }
        let settled = renderer.host().scroll_snapshot(scroll).unwrap();
        assert_eq!(settled.overscroll_y, 0.0);
        assert_eq!(settled.velocity_y, 0.0);
    }

    #[test]
    fn modal_scope_traps_focus_blocks_background_and_restores_focus() {
        let (window, view) = handles();
        let root = NodeId::new(0, 1);
        let background = NodeId::new(1, 1);
        let modal = NodeId::new(2, 1);
        let first = NodeId::new(3, 1);
        let second = NodeId::new(4, 1);
        let host = NativeUiHost::new(
            window,
            view,
            7,
            root,
            ProtocolLimits::default(),
            NativeUiHostConfig::default(),
        )
        .unwrap();
        let mut renderer = DesktopRenderer::new(host, 7, root, ProtocolLimits::default());
        renderer
            .apply(&MutationBatch::new(
                7,
                1,
                vec![
                    Mutation::Create {
                        id: background,
                        kind: NodeKind::Element(Primitive::Button),
                    },
                    Mutation::Listen {
                        id: background,
                        listener: Listener::new(EventType::POINTER_DOWN, HandlerId::new(1, 1)),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: background,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        renderer
            .host_mut()
            .set_layout_snapshot(
                1,
                [NativeUiLayout {
                    node: background,
                    rect: NativeUiRect::new(0.0, 0.0, 40.0, 40.0).unwrap(),
                    clip: None,
                    z_index: 0,
                }],
            )
            .unwrap();
        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 1,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 5_000,
                y_milli: 5_000,
            }))
            .unwrap());
        let _ = renderer.poll_event().unwrap();
        assert_eq!(renderer.host().focused_node(), Some(background));

        renderer
            .apply(&MutationBatch::new(
                7,
                2,
                vec![
                    Mutation::Create {
                        id: modal,
                        kind: NodeKind::Element(Primitive::Stack),
                    },
                    Mutation::SetProperty {
                        id: modal,
                        property: Property::new(PropertyId::MODAL, true),
                    },
                    Mutation::Listen {
                        id: modal,
                        listener: Listener::new(EventType::KEY_DOWN, HandlerId::new(2, 1)),
                    },
                    Mutation::Create {
                        id: first,
                        kind: NodeKind::Element(Primitive::Button),
                    },
                    Mutation::Create {
                        id: second,
                        kind: NodeKind::Element(Primitive::Button),
                    },
                    Mutation::InsertBefore {
                        parent: modal,
                        child: first,
                        before: None,
                    },
                    Mutation::InsertBefore {
                        parent: modal,
                        child: second,
                        before: None,
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: modal,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        assert_eq!(renderer.host().focused_node(), Some(first));

        let duplicate = NodeId::new(5, 1);
        let rejected = renderer.apply(&MutationBatch::new(
            7,
            3,
            vec![
                Mutation::Create {
                    id: duplicate,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::SetProperty {
                    id: duplicate,
                    property: Property::new(PropertyId::MODAL, true),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: duplicate,
                    before: None,
                },
            ],
        ));
        assert!(matches!(
            rejected,
            Err(vo_ui_desktop::DesktopRendererError::Host(
                NativeUiHostError::MultipleModalScopes
            ))
        ));
        assert_eq!(renderer.host().revision(), 2);

        assert!(!renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 2,
                physical_key: 48,
                logical_key: "Tab".to_string(),
                pressed: true,
                repeat: false,
                modifiers: NativeModifiers::default(),
            }))
            .unwrap());
        assert_eq!(renderer.host().focused_node(), Some(second));
        assert!(!renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 2,
                physical_key: 48,
                logical_key: "Tab".to_string(),
                pressed: true,
                repeat: false,
                modifiers: NativeModifiers {
                    shift: true,
                    ..NativeModifiers::default()
                },
            }))
            .unwrap());
        assert_eq!(renderer.host().focused_node(), Some(first));

        renderer
            .host_mut()
            .set_layout_snapshot(
                2,
                [
                    NativeUiLayout {
                        node: background,
                        rect: NativeUiRect::new(0.0, 0.0, 40.0, 40.0).unwrap(),
                        clip: None,
                        z_index: 0,
                    },
                    NativeUiLayout {
                        node: first,
                        rect: NativeUiRect::new(100.0, 0.0, 40.0, 40.0).unwrap(),
                        clip: None,
                        z_index: 2,
                    },
                ],
            )
            .unwrap();
        assert!(!renderer
            .host_mut()
            .route_input(&event(NativeInputKind::PointerButton {
                device: 1,
                button: NativePointerButton::Primary,
                pressed: true,
                click_count: 1,
                x_milli: 5_000,
                y_milli: 5_000,
            }))
            .unwrap());

        assert!(renderer
            .host_mut()
            .route_input(&event(NativeInputKind::Key {
                device: 2,
                physical_key: 53,
                logical_key: "Escape".to_string(),
                pressed: true,
                repeat: false,
                modifiers: NativeModifiers::default(),
            }))
            .unwrap());
        let escape = renderer.poll_event().unwrap().unwrap();
        assert_eq!(escape.target, modal);

        renderer
            .apply(&MutationBatch::new(
                7,
                3,
                vec![Mutation::RemoveProperty {
                    id: modal,
                    property: PropertyId::MODAL,
                }],
            ))
            .unwrap();
        assert_eq!(renderer.host().focused_node(), Some(background));
    }

    #[test]
    fn portal_contract_rejects_nested_and_out_of_range_layers_atomically() {
        let (window, view) = handles();
        let root = NodeId::new(0, 1);
        let host = NativeUiHost::new(
            window,
            view,
            7,
            root,
            ProtocolLimits::default(),
            NativeUiHostConfig::default(),
        )
        .unwrap();
        let mut renderer = DesktopRenderer::new(host, 7, root, ProtocolLimits::default());
        let portal = NodeId::new(1, 1);
        let nested = NodeId::new(2, 1);
        renderer
            .apply(&MutationBatch::new(
                7,
                1,
                vec![
                    Mutation::Create {
                        id: portal,
                        kind: NodeKind::Element(Primitive::Stack),
                    },
                    Mutation::SetProperty {
                        id: portal,
                        property: Property::new(PropertyId::PORTAL_LAYER, 200_i64),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: portal,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        let rejected = renderer.apply(&MutationBatch::new(
            7,
            2,
            vec![
                Mutation::Create {
                    id: nested,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::SetProperty {
                    id: nested,
                    property: Property::new(PropertyId::PORTAL_LAYER, 201_i64),
                },
                Mutation::InsertBefore {
                    parent: portal,
                    child: nested,
                    before: None,
                },
            ],
        ));
        assert!(matches!(
            rejected,
            Err(vo_ui_desktop::DesktopRendererError::Host(
                NativeUiHostError::NestedPortal(id)
            )) if id == nested
        ));
        assert_eq!(renderer.host().revision(), 1);

        let invalid = NodeId::new(3, 1);
        let rejected = renderer.apply(&MutationBatch::new(
            7,
            2,
            vec![
                Mutation::Create {
                    id: invalid,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::SetProperty {
                    id: invalid,
                    property: Property::new(PropertyId::PORTAL_LAYER, 1_000_001_i64),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: invalid,
                    before: None,
                },
            ],
        ));
        assert!(matches!(
            rejected,
            Err(vo_ui_desktop::DesktopRendererError::Host(
                NativeUiHostError::InvalidPortalLayer(id)
            )) if id == invalid
        ));
        assert_eq!(renderer.host().revision(), 1);
    }

    #[test]
    fn focus_request_tokens_are_composable_idempotent_and_reject_ambiguous_commits() {
        let (window, view) = handles();
        let root = NodeId::new(0, 1);
        let first = NodeId::new(1, 1);
        let second = NodeId::new(2, 1);
        let host = NativeUiHost::new(
            window,
            view,
            7,
            root,
            ProtocolLimits::default(),
            NativeUiHostConfig::default(),
        )
        .unwrap();
        let mut renderer = DesktopRenderer::new(host, 7, root, ProtocolLimits::default());
        renderer
            .apply(&MutationBatch::new(
                7,
                1,
                vec![
                    Mutation::Create {
                        id: first,
                        kind: NodeKind::Element(Primitive::TextInput),
                    },
                    Mutation::SetProperty {
                        id: first,
                        property: Property::new(PropertyId::FOCUS_REQUEST, 1_i64),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: first,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        assert_eq!(renderer.host().focused_node(), Some(first));

        renderer
            .apply(&MutationBatch::new(
                7,
                2,
                vec![
                    Mutation::Create {
                        id: second,
                        kind: NodeKind::Element(Primitive::Button),
                    },
                    Mutation::SetProperty {
                        id: second,
                        property: Property::new(PropertyId::FOCUS_REQUEST, 2_i64),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: second,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        assert_eq!(renderer.host().revision(), 2);
        assert_eq!(renderer.host().focused_node(), Some(second));

        let rejected = renderer.apply(&MutationBatch::new(
            7,
            3,
            vec![
                Mutation::SetProperty {
                    id: first,
                    property: Property::new(PropertyId::FOCUS_REQUEST, 3_i64),
                },
                Mutation::SetProperty {
                    id: second,
                    property: Property::new(PropertyId::FOCUS_REQUEST, 4_i64),
                },
            ],
        ));
        assert!(matches!(
            rejected,
            Err(vo_ui_desktop::DesktopRendererError::Host(
                NativeUiHostError::MultipleFocusRequests
            ))
        ));
        assert_eq!(renderer.host().revision(), 2);
        assert_eq!(renderer.host().focused_node(), Some(second));
    }

    #[test]
    fn layout_observation_quantizes_deduplicates_and_bounds_feedback() {
        let (window, view) = handles();
        let root = NodeId::new(0, 1);
        let observed = NodeId::new(1, 1);
        let host = NativeUiHost::new(
            window,
            view,
            7,
            root,
            ProtocolLimits::default(),
            NativeUiHostConfig {
                max_measurement_feedback_turns: 2,
                ..NativeUiHostConfig::default()
            },
        )
        .unwrap();
        let mut renderer = DesktopRenderer::new(host, 7, root, ProtocolLimits::default());
        renderer
            .apply(&MutationBatch::new(
                7,
                1,
                vec![
                    Mutation::Create {
                        id: observed,
                        kind: NodeKind::Element(Primitive::Box),
                    },
                    Mutation::SetProperty {
                        id: observed,
                        property: Property::new(PropertyId::WIDTH, 100_i64),
                    },
                    Mutation::SetProperty {
                        id: observed,
                        property: Property::new(PropertyId::HEIGHT, 40_i64),
                    },
                    Mutation::Listen {
                        id: observed,
                        listener: Listener::new(EventType::LAYOUT, HandlerId::new(9, 1)),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: observed,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        let mut measurer = ApproximateTextMeasurer;
        renderer
            .host_mut()
            .compute_and_set_layout(
                Size::new(320.0, 240.0),
                LayoutLimits::default(),
                &mut measurer,
            )
            .unwrap();
        let measured = renderer.poll_event().unwrap().unwrap();
        assert_eq!(measured.event, EventType::LAYOUT);
        assert!(matches!(
            measured.payload,
            EventPayload::Scroll(ScrollEventData {
                x: 100.0,
                y: 40.0,
                ..
            })
        ));
        renderer
            .host_mut()
            .compute_and_set_layout(
                Size::new(320.0, 240.0),
                LayoutLimits::default(),
                &mut measurer,
            )
            .unwrap();
        assert!(renderer.poll_event().unwrap().is_none());

        for (revision, width) in [(2, 101_i64), (3, 102_i64)] {
            renderer
                .apply(&MutationBatch::new(
                    7,
                    revision,
                    vec![Mutation::SetProperty {
                        id: observed,
                        property: Property::new(PropertyId::WIDTH, width),
                    }],
                ))
                .unwrap();
            renderer
                .host_mut()
                .compute_and_set_layout(
                    Size::new(320.0, 240.0),
                    LayoutLimits::default(),
                    &mut measurer,
                )
                .unwrap();
            assert_eq!(
                renderer.poll_event().unwrap().unwrap().event,
                EventType::LAYOUT
            );
        }
        renderer
            .apply(&MutationBatch::new(
                7,
                4,
                vec![Mutation::SetProperty {
                    id: observed,
                    property: Property::new(PropertyId::WIDTH, 103_i64),
                }],
            ))
            .unwrap();
        assert!(matches!(
            renderer.host_mut().compute_and_set_layout(
                Size::new(320.0, 240.0),
                LayoutLimits::default(),
                &mut measurer,
            ),
            Err(NativeUiLayoutError::Host(
                NativeUiHostError::MeasurementFeedbackLimitExceeded
            ))
        ));
    }
}
