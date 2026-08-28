#![no_std]

extern crate alloc;

use alloc::string::String;
use vo_ui_core::{EventType, HandlerId, Length, Listener, Primitive, PropertyId, Value, View};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Theme {
    pub background: u32,
    pub surface: u32,
    pub foreground: u32,
    pub muted_foreground: u32,
    pub accent: u32,
    pub accent_foreground: u32,
    pub danger: u32,
    pub radius_px: u16,
    pub spacing_px: u16,
}

impl Default for Theme {
    fn default() -> Self {
        Self {
            background: 0xfff7f7f8,
            surface: 0xffffffff,
            foreground: 0xff16181d,
            muted_foreground: 0xff646b76,
            accent: 0xff315efb,
            accent_foreground: 0xffffffff,
            danger: 0xffc4314b,
            radius_px: 8,
            spacing_px: 8,
        }
    }
}

pub fn text(value: impl Into<String>) -> View {
    View::text(value)
}

pub fn row(children: impl IntoIterator<Item = View>) -> View {
    View::element(Primitive::Row).children(children)
}

pub fn column(children: impl IntoIterator<Item = View>) -> View {
    View::element(Primitive::Column).children(children)
}

pub fn stack(children: impl IntoIterator<Item = View>) -> View {
    View::element(Primitive::Stack).children(children)
}

pub fn spacer() -> View {
    View::element(Primitive::Box).property(PropertyId::FLEX, 1_i64)
}

pub struct Button {
    label: String,
    handler: HandlerId,
    disabled: bool,
    accessible_name: Option<String>,
}

impl Button {
    pub fn new(label: impl Into<String>, handler: HandlerId) -> Self {
        Self {
            label: label.into(),
            handler,
            disabled: false,
            accessible_name: None,
        }
    }

    pub fn disabled(mut self, disabled: bool) -> Self {
        self.disabled = disabled;
        self
    }

    pub fn accessible_name(mut self, name: impl Into<String>) -> Self {
        self.accessible_name = Some(name.into());
        self
    }

    pub fn view(self, theme: &Theme) -> View {
        let accessible_name = self.accessible_name.unwrap_or_else(|| self.label.clone());
        View::element(Primitive::Button)
            .property(PropertyId::DISABLED, self.disabled)
            .property(PropertyId::ROLE, "button")
            .property(PropertyId::ACCESSIBLE_NAME, accessible_name)
            .property(PropertyId::BACKGROUND, Value::Color(theme.accent))
            .property(
                PropertyId::FOREGROUND,
                Value::Color(theme.accent_foreground),
            )
            .property(PropertyId::RADIUS, i64::from(theme.radius_px))
            .listener(Listener::new(EventType::CLICK, self.handler))
            .child(View::text(self.label))
    }
}

pub struct TextInput {
    value: String,
    input_handler: HandlerId,
    placeholder: Option<String>,
    accessible_name: String,
    disabled: bool,
}

impl TextInput {
    pub fn new(
        accessible_name: impl Into<String>,
        value: impl Into<String>,
        input_handler: HandlerId,
    ) -> Self {
        Self {
            value: value.into(),
            input_handler,
            placeholder: None,
            accessible_name: accessible_name.into(),
            disabled: false,
        }
    }

    pub fn placeholder(mut self, value: impl Into<String>) -> Self {
        self.placeholder = Some(value.into());
        self
    }

    pub fn disabled(mut self, disabled: bool) -> Self {
        self.disabled = disabled;
        self
    }

    pub fn view(self) -> View {
        let mut view = View::element(Primitive::TextInput)
            .property(PropertyId::VALUE, self.value)
            .property(PropertyId::ROLE, "textbox")
            .property(PropertyId::ACCESSIBLE_NAME, self.accessible_name)
            .property(PropertyId::DISABLED, self.disabled)
            .listener(Listener::new(EventType::INPUT, self.input_handler));
        if let Some(placeholder) = self.placeholder {
            view = view.property(PropertyId::PLACEHOLDER, placeholder);
        }
        view
    }
}

pub fn card(child: View, theme: &Theme) -> View {
    View::element(Primitive::Box)
        .property(PropertyId::BACKGROUND, Value::Color(theme.surface))
        .property(
            PropertyId::PADDING,
            Length::Px(f32::from(theme.spacing_px) * 2.0),
        )
        .property(PropertyId::RADIUS, i64::from(theme.radius_px))
        .child(child)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn button_has_accessibility_and_typed_click_binding() {
        let handler = HandlerId::new(3, 2);
        let view = Button::new("Save", handler).view(&Theme::default());
        assert!(view.properties.iter().any(|property| {
            property.id == PropertyId::ACCESSIBLE_NAME
                && property.value == Value::Text(String::from("Save"))
        }));
        assert_eq!(view.listeners[0].handler, handler);
        assert_eq!(view.listeners[0].event, EventType::CLICK);
    }
}
