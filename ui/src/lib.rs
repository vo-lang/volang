#![no_std]

pub use vo_ui_accessibility as accessibility;
pub use vo_ui_artifact as artifact;
pub use vo_ui_core as core;
pub use vo_ui_desktop as desktop;
pub use vo_ui_kit as kit;
pub use vo_ui_layout as layout;
pub use vo_ui_paint as paint;
pub use vo_ui_plan as plan;
pub use vo_ui_protocol as protocol;
pub use vo_ui_reactive as reactive;
pub use vo_ui_reload as reload;
pub use vo_ui_runtime as runtime;
pub use vo_ui_scheduler as scheduler;
pub use vo_ui_session as session;
pub use vo_ui_system as system;
pub use vo_ui_web as web;

pub mod prelude {
    pub use vo_ui_accessibility::{
        build_accessibility_tree, AccessibilityActions, AccessibilityError, AccessibilityLimits,
        AccessibilityNode, AccessibilityRole, AccessibilityState, AccessibilityTree,
    };
    pub use vo_ui_core::{
        EventModifiers, EventType, HandlerId, Key, KeyEventData, Length, Listener, NodeId,
        PointerEventData, PointerKind, Primitive, PropertyId, ScrollEventData, ScrollUnit, Value,
        View,
    };
    pub use vo_ui_desktop::{
        DesktopElement, DesktopEventError, DesktopHost, DesktopPollError, DesktopRenderer,
        DesktopRendererError,
    };
    pub use vo_ui_kit::{card, column, row, spacer, stack, text, Button, TextInput, Theme};
    pub use vo_ui_layout::{
        compute_layout, ApproximateTextMeasurer, IntrinsicMeasurer, LayoutBox, LayoutError,
        LayoutLimits, LayoutSnapshot, Rect, Size,
    };
    pub use vo_ui_paint::{build_paint_scene, DrawCommand, PaintError, PaintLimits, PaintScene};
    pub use vo_ui_plan::{ComponentPlan, LocalNodeId, SlotId, SlotKind, SlotValue, ValidatedPlan};
    pub use vo_ui_protocol::{decode_event, encode_event, EventEnvelope};
    pub use vo_ui_reactive::{Lane, Runtime as ReactiveRuntime, ScopeId, Signal};
    pub use vo_ui_reload::{
        plan_reload, ComponentSchema, ReloadLimits, ReloadPlan, StateAction, StateField,
    };
    pub use vo_ui_runtime::{TemplateRuntime, UiRuntime};
    pub use vo_ui_scheduler::{SchedulerConfig, UiScheduler};
    pub use vo_ui_session::{CommitReport, MessageHandler, SessionTurn, SlotWrites, UiSession};
    pub use vo_ui_web::{
        DomBatch, DomCommand, DomElement, DomEvent, DomHost, DomPollError, DomRenderer, WebWire,
        WireDomHost,
    };
}
