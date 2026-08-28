#![no_std]

extern crate alloc;

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;

macro_rules! generational_id {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name {
            index: u32,
            generation: u32,
        }

        impl $name {
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
    };
}

generational_id!(NodeId);
generational_id!(HandlerId);
generational_id!(TaskId);
generational_id!(ResourceId);

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Key {
    Integer(i64),
    Text(String),
}

impl From<i64> for Key {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<&str> for Key {
    fn from(value: &str) -> Self {
        Self::Text(value.to_string())
    }
}

impl From<String> for Key {
    fn from(value: String) -> Self {
        Self::Text(value)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(u16)]
pub enum Primitive {
    Root = 0,
    Fragment = 1,
    Box = 2,
    Row = 3,
    Column = 4,
    Stack = 5,
    Grid = 6,
    Scroll = 7,
    Image = 8,
    Button = 9,
    TextInput = 10,
    Toggle = 11,
    Slider = 12,
    Canvas = 13,
    PlatformView = 14,
    Text = 15,
    TextArea = 16,
}

impl Primitive {
    pub const fn from_code(code: u16) -> Option<Self> {
        Some(match code {
            0 => Self::Root,
            1 => Self::Fragment,
            2 => Self::Box,
            3 => Self::Row,
            4 => Self::Column,
            5 => Self::Stack,
            6 => Self::Grid,
            7 => Self::Scroll,
            8 => Self::Image,
            9 => Self::Button,
            10 => Self::TextInput,
            11 => Self::Toggle,
            12 => Self::Slider,
            13 => Self::Canvas,
            14 => Self::PlatformView,
            15 => Self::Text,
            16 => Self::TextArea,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PropertyId(pub u32);

impl PropertyId {
    pub const WIDTH: Self = Self(1);
    pub const HEIGHT: Self = Self(2);
    pub const MIN_WIDTH: Self = Self(3);
    pub const MIN_HEIGHT: Self = Self(4);
    pub const MAX_WIDTH: Self = Self(5);
    pub const MAX_HEIGHT: Self = Self(6);
    pub const FLEX: Self = Self(7);
    pub const GAP: Self = Self(8);
    pub const PADDING: Self = Self(9);
    pub const BACKGROUND: Self = Self(10);
    pub const FOREGROUND: Self = Self(11);
    pub const FONT_SIZE: Self = Self(12);
    pub const FONT_WEIGHT: Self = Self(13);
    pub const ALIGN: Self = Self(14);
    pub const JUSTIFY: Self = Self(15);
    pub const VALUE: Self = Self(16);
    pub const PLACEHOLDER: Self = Self(17);
    pub const DISABLED: Self = Self(18);
    pub const ROLE: Self = Self(19);
    pub const ACCESSIBLE_NAME: Self = Self(20);
    pub const TEST_ID: Self = Self(21);
    pub const GRID_COLUMNS: Self = Self(22);
    pub const OVERFLOW: Self = Self(23);
    pub const RADIUS: Self = Self(24);
    pub const CHECKED: Self = Self(25);
    pub const SCROLL_X: Self = Self(26);
    pub const SCROLL_Y: Self = Self(27);
    pub const REQUIRED: Self = Self(28);
    pub const INVALID: Self = Self(29);
    pub const ACCESSIBLE_DESCRIPTION: Self = Self(30);
    pub const SELECTION_START_UTF16: Self = Self(31);
    pub const SELECTION_LENGTH_UTF16: Self = Self(32);
    pub const GRID_TEMPLATE_AREAS: Self = Self(33);
    pub const GRID_AREA: Self = Self(34);
    /// Declares the single active modal focus scope for a committed tree.
    pub const MODAL: Self = Self(35);
    /// Selects the preferred initial focus target inside a modal scope.
    pub const AUTO_FOCUS: Self = Self(36);
    /// Controls platform hit testing with the portable values `auto` and `none`.
    pub const POINTER_EVENTS: Self = Self(37);
    /// Captures an active pointer to this element until up or cancellation.
    pub const POINTER_CAPTURE: Self = Self(38);
    /// Inherited logical flow direction: 0 is LTR and 1 is RTL.
    pub const FLOW_DIRECTION: Self = Self(39);
    /// Reparents the logical subtree into a host overlay plane. The signed
    /// value is the deterministic layer; equal layers preserve tree order.
    pub const PORTAL_LAYER: Self = Self(40);
    /// Monotonic application token requesting post-commit focus for one node.
    /// Zero disables the request; equal node/token pairs are idempotent.
    pub const FOCUS_REQUEST: Self = Self(41);
    /// Semantic selection state for tabs, options, rows, and navigation items.
    pub const SELECTED: Self = Self(42);
    /// Semantic disclosure state for menus, popovers, comboboxes, and trees.
    pub const EXPANDED: Self = Self(43);
    /// Semantic pressed state for toggle buttons.
    pub const PRESSED: Self = Self(44);
    /// ARIA-current compatible token: true, page, step, location, date, or time.
    pub const CURRENT: Self = Self(45);
    /// Removes a subtree from layout, paint, hit testing, focus, and semantics
    /// while retaining its reconciler identity and controlled state.
    pub const HIDDEN: Self = Self(46);
    /// Same-origin, application-relative, data, or capability-resolved source.
    pub const SOURCE: Self = Self(47);
    /// A bounded media or platform-view content-type token.
    pub const CONTENT_TYPE: Self = Self(48);
    /// Portable asset fit: contain, cover, fill, none, or scale-down.
    pub const FIT: Self = Self(49);
    /// Finite opacity in the inclusive range zero through one.
    pub const OPACITY: Self = Self(50);
    /// Portable affine transform encoded as six finite comma-separated values.
    pub const TRANSFORM: Self = Self(51);
    /// Versioned, bounded renderer-neutral graphics command stream.
    pub const GRAPHICS_PROGRAM: Self = Self(52);
    /// Versioned, bounded platform media state projection.
    pub const MEDIA_STATE: Self = Self(53);
    /// Optional source displayed before media playback is ready.
    pub const POSTER: Self = Self(54);
    /// Solid renderer-neutral border color. A border is painted only when a
    /// positive `BORDER_WIDTH` is present on the same node.
    pub const BORDER_COLOR: Self = Self(55);
    /// Finite non-negative solid border width in logical pixels.
    pub const BORDER_WIDTH: Self = Self(56);
    /// Numeric lower bound for range-like controls.
    pub const MIN_VALUE: Self = Self(57);
    /// Numeric upper bound for range-like controls.
    pub const MAX_VALUE: Self = Self(58);
    /// Positive numeric increment for range-like controls.
    pub const STEP_VALUE: Self = Self(59);
    /// Excludes a visible subtree from platform accessibility projections.
    /// This is intended for duplicated visual layers such as syntax paint.
    pub const ACCESSIBILITY_HIDDEN: Self = Self(60);
    /// Adds or removes an authored node from sequential keyboard focus.
    pub const FOCUSABLE: Self = Self(61);

    pub const CUSTOM_START: u32 = 1 << 16;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Length {
    Auto,
    Px(f32),
    Percent(f32),
    ViewportWidth(f32),
    ViewportHeight(f32),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    I64(i64),
    F64(f64),
    Text(String),
    Color(u32),
    Length(Length),
    Bytes(Vec<u8>),
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::I64(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::F64(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Text(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::Text(value.to_string())
    }
}

impl From<Length> for Value {
    fn from(value: Length) -> Self {
        Self::Length(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Property {
    pub id: PropertyId,
    pub value: Value,
}

impl Property {
    pub fn new(id: PropertyId, value: impl Into<Value>) -> Self {
        Self {
            id,
            value: value.into(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct EventType(pub u16);

impl EventType {
    pub const CLICK: Self = Self(1);
    pub const INPUT: Self = Self(2);
    pub const CHANGE: Self = Self(3);
    pub const SUBMIT: Self = Self(4);
    pub const FOCUS: Self = Self(5);
    pub const BLUR: Self = Self(6);
    pub const KEY_DOWN: Self = Self(7);
    pub const KEY_UP: Self = Self(8);
    pub const POINTER_DOWN: Self = Self(9);
    pub const POINTER_MOVE: Self = Self(10);
    pub const POINTER_UP: Self = Self(11);
    pub const SCROLL: Self = Self(12);
    pub const COMPOSITION_START: Self = Self(13);
    pub const COMPOSITION_UPDATE: Self = Self(14);
    pub const COMPOSITION_END: Self = Self(15);
    pub const WHEEL: Self = Self(16);
    /// Renderer-neutral wake used to commit state written by worker
    /// goroutines. It uses the reserved `u32::MAX` handler sentinel and never
    /// invokes an application closure.
    pub const INVALIDATE: Self = Self(17);
    pub const POINTER_CANCEL: Self = Self(18);
    /// Post-layout size notification emitted after a committed revision.
    pub const LAYOUT: Self = Self(19);
    /// Controlled text selection changed without requiring a text edit.
    pub const SELECTION_CHANGE: Self = Self(20);
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct EventModifiers {
    pub shift: bool,
    pub control: bool,
    pub alt: bool,
    pub meta: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct KeyEventData {
    pub key: String,
    pub code: String,
    pub modifiers: EventModifiers,
    pub repeat: bool,
    pub composing: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum PointerKind {
    Unknown = 0,
    Mouse = 1,
    Pen = 2,
    Touch = 3,
}

impl PointerKind {
    pub const fn from_code(code: u8) -> Option<Self> {
        Some(match code {
            0 => Self::Unknown,
            1 => Self::Mouse,
            2 => Self::Pen,
            3 => Self::Touch,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PointerEventData {
    pub x: f64,
    pub y: f64,
    pub button: i16,
    pub buttons: u16,
    pub pointer_id: i64,
    pub kind: PointerKind,
    pub modifiers: EventModifiers,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum ScrollUnit {
    Pixel = 0,
    Line = 1,
    Page = 2,
}

impl ScrollUnit {
    pub const fn from_code(code: u8) -> Option<Self> {
        Some(match code {
            0 => Self::Pixel,
            1 => Self::Line,
            2 => Self::Page,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ScrollEventData {
    pub x: f64,
    pub y: f64,
    pub delta_x: f64,
    pub delta_y: f64,
    pub unit: ScrollUnit,
    pub modifiers: EventModifiers,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompositionEventData {
    pub text: String,
    pub selection_start_utf16: u32,
    pub selection_length_utf16: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TextInputEventData {
    pub text: String,
    pub selection_start_utf16: u32,
    pub selection_length_utf16: u32,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ListenerOptions {
    pub capture: bool,
    pub passive: bool,
    pub once: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Listener {
    pub event: EventType,
    pub handler: HandlerId,
    pub options: ListenerOptions,
}

impl Listener {
    pub const fn new(event: EventType, handler: HandlerId) -> Self {
        Self {
            event,
            handler,
            options: ListenerOptions {
                capture: false,
                passive: false,
                once: false,
            },
        }
    }

    pub const fn with_options(mut self, options: ListenerOptions) -> Self {
        self.options = options;
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ViewKind {
    Element(Primitive),
    Text(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct View {
    pub key: Option<Key>,
    pub kind: ViewKind,
    pub properties: Vec<Property>,
    pub listeners: Vec<Listener>,
    pub children: Vec<View>,
}

impl View {
    pub fn element(primitive: Primitive) -> Self {
        Self {
            key: None,
            kind: ViewKind::Element(primitive),
            properties: Vec::new(),
            listeners: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn fragment(children: impl IntoIterator<Item = View>) -> Self {
        Self::element(Primitive::Fragment).children(children)
    }

    pub fn text(value: impl Into<String>) -> Self {
        Self {
            key: None,
            kind: ViewKind::Text(value.into()),
            properties: Vec::new(),
            listeners: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn key(mut self, key: impl Into<Key>) -> Self {
        self.key = Some(key.into());
        self
    }

    pub fn property(mut self, id: PropertyId, value: impl Into<Value>) -> Self {
        let value = value.into();
        if let Some(property) = self
            .properties
            .iter_mut()
            .find(|property| property.id == id)
        {
            property.value = value;
        } else {
            self.properties.push(Property { id, value });
        }
        self
    }

    pub fn listener(mut self, listener: Listener) -> Self {
        if let Some(current) = self
            .listeners
            .iter_mut()
            .find(|current| current.event == listener.event)
        {
            *current = listener;
        } else {
            self.listeners.push(listener);
        }
        self
    }

    pub fn child(mut self, child: View) -> Self {
        self.children.push(child);
        self
    }

    pub fn children(mut self, children: impl IntoIterator<Item = View>) -> Self {
        self.children.extend(children);
        self
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum EventPayload {
    None,
    Text(String),
    Toggle(bool),
    Scalar(i64),
    Bytes(Vec<u8>),
    Key(KeyEventData),
    Pointer(PointerEventData),
    Scroll(ScrollEventData),
    Composition(CompositionEventData),
    TextInput(TextInputEventData),
}

#[derive(Clone, Debug, PartialEq)]
pub struct UiEvent {
    pub handler: HandlerId,
    pub event: EventType,
    pub target: NodeId,
    pub sequence: u64,
    pub payload: EventPayload,
}

impl fmt::Display for NodeId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}:{}", self.index, self.generation)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn view_builder_replaces_duplicate_property_and_listener() {
        let first = HandlerId::new(1, 1);
        let second = HandlerId::new(2, 1);
        let view = View::element(Primitive::Button)
            .property(PropertyId::DISABLED, false)
            .property(PropertyId::DISABLED, true)
            .listener(Listener::new(EventType::CLICK, first))
            .listener(Listener::new(EventType::CLICK, second));

        assert_eq!(view.properties.len(), 1);
        assert_eq!(view.properties[0].value, Value::Bool(true));
        assert_eq!(view.listeners.len(), 1);
        assert_eq!(view.listeners[0].handler, second);
    }
}
