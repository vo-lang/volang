#![no_std]

extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{Length, NodeId, PropertyId, Value};
use vo_ui_layout::{LayoutSnapshot, Rect, Size};
use vo_ui_protocol::{NodeKind, NodeSnapshot, TreeMirror};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PaintLimits {
    pub max_commands: usize,
    pub max_depth: usize,
    pub max_text_bytes: usize,
}

impl Default for PaintLimits {
    fn default() -> Self {
        Self {
            max_commands: 200_000,
            max_depth: 1_024,
            max_text_bytes: 16 * 1024 * 1024,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DrawCommand {
    FillRect {
        node: NodeId,
        rect: Rect,
        clip: Option<Rect>,
        color: u32,
        radius: f64,
    },
    StrokeRect {
        node: NodeId,
        rect: Rect,
        clip: Option<Rect>,
        color: u32,
        radius: f64,
        width: f64,
    },
    Text {
        node: NodeId,
        rect: Rect,
        clip: Option<Rect>,
        color: u32,
        font_size: f64,
        font_weight: i64,
        value: String,
    },
    TextEditor {
        node: NodeId,
        rect: Rect,
        clip: Option<Rect>,
        color: u32,
        font_size: f64,
        font_weight: i64,
        value: String,
        placeholder: String,
        selection_start_utf16: u32,
        selection_length_utf16: u32,
    },
    Scrollbar {
        node: NodeId,
        track: Rect,
        thumb: Rect,
        color: u32,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct PaintScene {
    pub revision: u64,
    pub viewport: Size,
    commands: Vec<DrawCommand>,
}

impl PaintScene {
    pub fn commands(&self) -> &[DrawCommand] {
        &self.commands
    }

    pub fn len(&self) -> usize {
        self.commands.len()
    }

    pub fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PaintError {
    InvalidLimits,
    RevisionMismatch,
    MissingNode(NodeId),
    MissingLayout(NodeId),
    InvalidProperty(NodeId, PropertyId),
    CommandLimitExceeded,
    TextLimitExceeded,
    DepthLimitExceeded,
    InvalidGraphicsProgram(NodeId),
}

impl fmt::Display for PaintError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "UI paint scene generation failed: {self:?}")
    }
}

#[derive(Clone, Copy)]
struct InheritedStyle {
    foreground: u32,
    font_size: f64,
    font_weight: i64,
    opacity: f64,
}

struct SceneBuilder<'a> {
    tree: &'a TreeMirror,
    layout: &'a LayoutSnapshot,
    limits: PaintLimits,
    commands: Vec<DrawCommand>,
    text_bytes: usize,
}

pub fn build_paint_scene(
    tree: &TreeMirror,
    layout: &LayoutSnapshot,
    limits: PaintLimits,
) -> Result<PaintScene, PaintError> {
    if limits.max_commands == 0 || limits.max_depth == 0 || limits.max_text_bytes == 0 {
        return Err(PaintError::InvalidLimits);
    }
    if tree.revision() != layout.revision {
        return Err(PaintError::RevisionMismatch);
    }
    let mut builder = SceneBuilder {
        tree,
        layout,
        limits,
        commands: Vec::new(),
        text_bytes: 0,
    };
    builder.visit(
        tree.root(),
        0,
        InheritedStyle {
            foreground: 0xff000000,
            font_size: 14.0,
            font_weight: 400,
            opacity: 1.0,
        },
    )?;
    builder.push_scrollbars()?;
    builder.order_overlay_planes()?;
    Ok(PaintScene {
        revision: layout.revision,
        viewport: layout.viewport,
        commands: builder.commands,
    })
}

impl SceneBuilder<'_> {
    fn visit(
        &mut self,
        id: NodeId,
        depth: usize,
        inherited: InheritedStyle,
    ) -> Result<(), PaintError> {
        if depth >= self.limits.max_depth {
            return Err(PaintError::DepthLimitExceeded);
        }
        let node = self.tree.node(id).ok_or(PaintError::MissingNode(id))?;
        if boolean_property(&node, PropertyId::HIDDEN, id)?.unwrap_or(false) {
            return Ok(());
        }
        let layout = self
            .layout
            .get(id)
            .copied()
            .ok_or(PaintError::MissingLayout(id))?;
        let local_opacity = number_property(&node, PropertyId::OPACITY, id)?.unwrap_or(1.0);
        if !local_opacity.is_finite() || !(0.0..=1.0).contains(&local_opacity) {
            return Err(PaintError::InvalidProperty(id, PropertyId::OPACITY));
        }
        let style = InheritedStyle {
            foreground: color_property(&node, PropertyId::FOREGROUND, id)?
                .unwrap_or(inherited.foreground),
            font_size: number_property(&node, PropertyId::FONT_SIZE, id)?
                .unwrap_or(inherited.font_size),
            font_weight: integer_property(&node, PropertyId::FONT_WEIGHT, id)?
                .unwrap_or(inherited.font_weight),
            opacity: inherited.opacity * local_opacity,
        };
        if node.kind != NodeKind::Element(vo_ui_core::Primitive::Slider) {
            if let Some(color) = color_property(&node, PropertyId::BACKGROUND, id)? {
                self.push(DrawCommand::FillRect {
                    node: id,
                    rect: layout.rect,
                    clip: layout.clip,
                    color: color_with_opacity(color, style.opacity),
                    radius: number_property(&node, PropertyId::RADIUS, id)?.unwrap_or(0.0),
                })?;
            }
        }
        if let Some(color) = color_property(&node, PropertyId::BORDER_COLOR, id)? {
            let width = number_property(&node, PropertyId::BORDER_WIDTH, id)?.unwrap_or(0.0);
            if !width.is_finite() || width < 0.0 {
                return Err(PaintError::InvalidProperty(id, PropertyId::BORDER_WIDTH));
            }
            if width > 0.0 {
                self.push(DrawCommand::StrokeRect {
                    node: id,
                    rect: layout.rect,
                    clip: layout.clip,
                    color: color_with_opacity(color, style.opacity),
                    radius: number_property(&node, PropertyId::RADIUS, id)?.unwrap_or(0.0),
                    width,
                })?;
            }
        }
        if node.kind == NodeKind::Text {
            self.text_bytes = self
                .text_bytes
                .checked_add(node.text.len())
                .ok_or(PaintError::TextLimitExceeded)?;
            if self.text_bytes > self.limits.max_text_bytes {
                return Err(PaintError::TextLimitExceeded);
            }
            self.push(DrawCommand::Text {
                node: id,
                rect: layout.content,
                clip: layout.clip,
                color: color_with_opacity(style.foreground, style.opacity),
                font_size: style.font_size,
                font_weight: style.font_weight,
                value: node.text.clone(),
            })?;
        }
        if matches!(
            node.kind,
            NodeKind::Element(vo_ui_core::Primitive::TextInput | vo_ui_core::Primitive::TextArea)
        ) {
            let value = string_property(&node, PropertyId::VALUE, id)?.unwrap_or_default();
            let placeholder =
                string_property(&node, PropertyId::PLACEHOLDER, id)?.unwrap_or_default();
            self.text_bytes = self
                .text_bytes
                .checked_add(value.len())
                .and_then(|bytes| bytes.checked_add(placeholder.len()))
                .ok_or(PaintError::TextLimitExceeded)?;
            if self.text_bytes > self.limits.max_text_bytes {
                return Err(PaintError::TextLimitExceeded);
            }
            self.push(DrawCommand::TextEditor {
                node: id,
                rect: layout.content,
                clip: layout.clip,
                color: color_with_opacity(style.foreground, style.opacity),
                font_size: style.font_size,
                font_weight: style.font_weight,
                value,
                placeholder,
                selection_start_utf16: unsigned_property(
                    &node,
                    PropertyId::SELECTION_START_UTF16,
                    id,
                )?
                .unwrap_or(0),
                selection_length_utf16: unsigned_property(
                    &node,
                    PropertyId::SELECTION_LENGTH_UTF16,
                    id,
                )?
                .unwrap_or(0),
            })?;
        }
        if node.kind == NodeKind::Element(vo_ui_core::Primitive::Toggle) {
            let checked = boolean_property(&node, PropertyId::CHECKED, id)?.unwrap_or(false);
            let bounds = layout.content;
            let role = string_property(&node, PropertyId::ROLE, id)?.unwrap_or_default();
            if role == "checkbox" || role == "menuitemcheckbox" {
                let size = bounds.width.min(bounds.height).clamp(1.0, 20.0);
                let indicator = Rect::new(
                    bounds.x,
                    bounds.y + (bounds.height - size) / 2.0,
                    size,
                    size,
                );
                self.push(DrawCommand::FillRect {
                    node: id,
                    rect: indicator,
                    clip: layout.clip,
                    color: color_with_opacity(
                        if checked {
                            style.foreground
                        } else {
                            0xffffffff
                        },
                        style.opacity,
                    ),
                    radius: (size * 0.22).min(4.0),
                })?;
                self.push(DrawCommand::StrokeRect {
                    node: id,
                    rect: indicator,
                    clip: layout.clip,
                    color: color_with_opacity(
                        if checked {
                            style.foreground
                        } else {
                            0xff64748b
                        },
                        style.opacity,
                    ),
                    radius: (size * 0.22).min(4.0),
                    width: 1.5_f64.min(size),
                })?;
                if checked {
                    let inset = (size * 0.3).max(1.0);
                    self.push(DrawCommand::FillRect {
                        node: id,
                        rect: Rect::new(
                            indicator.x + inset,
                            indicator.y + inset,
                            (size - inset * 2.0).max(1.0),
                            (size - inset * 2.0).max(1.0),
                        ),
                        clip: layout.clip,
                        color: color_with_opacity(0xffffffff, style.opacity),
                        radius: 1.0,
                    })?;
                }
            } else {
                let track_height = bounds.height.clamp(1.0, 24.0);
                let track_width = bounds.width.min(40.0).max(track_height);
                let track = Rect::new(
                    bounds.x,
                    bounds.y + (bounds.height - track_height) / 2.0,
                    track_width,
                    track_height,
                );
                self.push(DrawCommand::FillRect {
                    node: id,
                    rect: track,
                    clip: layout.clip,
                    color: color_with_opacity(
                        if checked {
                            style.foreground
                        } else {
                            0xff64748b
                        },
                        style.opacity,
                    ),
                    radius: track_height / 2.0,
                })?;
                let inset = (track_height * 0.125).max(1.0);
                let thumb = (track_height - inset * 2.0).max(1.0);
                let thumb_x = if checked {
                    track.x + track.width - inset - thumb
                } else {
                    track.x + inset
                };
                self.push(DrawCommand::FillRect {
                    node: id,
                    rect: Rect::new(thumb_x, track.y + inset, thumb, thumb),
                    clip: layout.clip,
                    color: color_with_opacity(0xffffffff, style.opacity),
                    radius: thumb / 2.0,
                })?;
            }
        }
        if node.kind == NodeKind::Element(vo_ui_core::Primitive::Slider) {
            let minimum = number_property(&node, PropertyId::MIN_VALUE, id)?.unwrap_or(0.0);
            let maximum = number_property(&node, PropertyId::MAX_VALUE, id)?.unwrap_or(100.0);
            let value = number_property(&node, PropertyId::VALUE, id)?.unwrap_or(minimum);
            let step = number_property(&node, PropertyId::STEP_VALUE, id)?.unwrap_or(1.0);
            if !minimum.is_finite()
                || !maximum.is_finite()
                || !value.is_finite()
                || !step.is_finite()
                || maximum <= minimum
                || step <= 0.0
                || value < minimum
                || value > maximum
            {
                return Err(PaintError::InvalidProperty(id, PropertyId::VALUE));
            }
            let track_height = 4.0_f64.min(layout.content.height);
            let track = Rect::new(
                layout.content.x,
                layout.content.y + (layout.content.height - track_height) / 2.0,
                layout.content.width,
                track_height,
            );
            let progress = (value - minimum) / (maximum - minimum);
            let track_color =
                color_property(&node, PropertyId::BACKGROUND, id)?.unwrap_or(0xff64748b);
            self.push(DrawCommand::FillRect {
                node: id,
                rect: track,
                clip: layout.clip,
                color: color_with_opacity(track_color, style.opacity),
                radius: track_height / 2.0,
            })?;
            self.push(DrawCommand::FillRect {
                node: id,
                rect: Rect::new(track.x, track.y, track.width * progress, track.height),
                clip: layout.clip,
                color: color_with_opacity(style.foreground, style.opacity),
                radius: track_height / 2.0,
            })?;
            let thumb = 16.0_f64.min(layout.content.height.max(1.0));
            let center = track.x + track.width * progress;
            self.push(DrawCommand::FillRect {
                node: id,
                rect: Rect::new(
                    center - thumb / 2.0,
                    layout.content.y + (layout.content.height - thumb) / 2.0,
                    thumb,
                    thumb,
                ),
                clip: layout.clip,
                color: color_with_opacity(style.foreground, style.opacity),
                radius: thumb / 2.0,
            })?;
        }
        if node.kind == NodeKind::Element(vo_ui_core::Primitive::Canvas) {
            if let Some(program) = string_property(&node, PropertyId::GRAPHICS_PROGRAM, id)? {
                self.push_graphics_program(
                    id,
                    layout.content,
                    layout.clip,
                    &program,
                    style.opacity,
                )?;
            }
        }
        for child in node.children {
            self.visit(child, depth + 1, style)?;
        }
        Ok(())
    }

    fn push_graphics_program(
        &mut self,
        node: NodeId,
        bounds: Rect,
        clip: Option<Rect>,
        program: &str,
        opacity: f64,
    ) -> Result<(), PaintError> {
        if program.len() > 1_048_576 {
            return Err(PaintError::InvalidGraphicsProgram(node));
        }
        let mut lines = program.lines();
        if lines.next() != Some("VGC1") {
            return Err(PaintError::InvalidGraphicsProgram(node));
        }
        for (index, line) in lines.enumerate() {
            if index >= 65_536 {
                return Err(PaintError::CommandLimitExceeded);
            }
            let mut fields = line.split('|');
            let kind = fields
                .next()
                .and_then(|value| value.parse::<u8>().ok())
                .ok_or(PaintError::InvalidGraphicsProgram(node))?;
            let values = fields
                .next()
                .ok_or(PaintError::InvalidGraphicsProgram(node))?
                .split(',')
                .filter(|value| !value.is_empty())
                .map(|value| value.parse::<f64>())
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| PaintError::InvalidGraphicsProgram(node))?;
            if values.iter().any(|value| !value.is_finite()) {
                return Err(PaintError::InvalidGraphicsProgram(node));
            }
            let color = fields
                .next()
                .filter(|value| !value.is_empty() && value.len() <= 8)
                .and_then(|value| u32::from_str_radix(value, 16).ok())
                .ok_or(PaintError::InvalidGraphicsProgram(node))?;
            let color = color_with_opacity(color, opacity);
            let width = fields
                .next()
                .and_then(|value| value.parse::<f64>().ok())
                .filter(|value| value.is_finite() && *value >= 0.0)
                .ok_or(PaintError::InvalidGraphicsProgram(node))?;
            let payload = fields
                .next()
                .ok_or(PaintError::InvalidGraphicsProgram(node))?;
            if fields.next().is_some() {
                return Err(PaintError::InvalidGraphicsProgram(node));
            }
            let local_rect = |x: f64, y: f64, width: f64, height: f64| {
                Rect::new(bounds.x + x, bounds.y + y, width.max(0.0), height.max(0.0))
            };
            match (kind, values.as_slice()) {
                (0, []) => self.push(DrawCommand::FillRect {
                    node,
                    rect: bounds,
                    clip,
                    color,
                    radius: 0.0,
                })?,
                (1, [x, y, width, height]) => self.push(DrawCommand::FillRect {
                    node,
                    rect: local_rect(*x, *y, *width, *height),
                    clip,
                    color,
                    radius: 0.0,
                })?,
                (2, [x, y, rectangle_width, rectangle_height]) => {
                    let stroke = width.max(1.0);
                    for rect in [
                        local_rect(*x, *y, *rectangle_width, stroke),
                        local_rect(
                            *x,
                            *y + *rectangle_height - stroke,
                            *rectangle_width,
                            stroke,
                        ),
                        local_rect(*x, *y, stroke, *rectangle_height),
                        local_rect(
                            *x + *rectangle_width - stroke,
                            *y,
                            stroke,
                            *rectangle_height,
                        ),
                    ] {
                        self.push(DrawCommand::FillRect {
                            node,
                            rect,
                            clip,
                            color,
                            radius: 0.0,
                        })?;
                    }
                }
                (3, [x1, y1, x2, y2]) => self.push(DrawCommand::FillRect {
                    node,
                    rect: local_rect(
                        x1.min(*x2),
                        y1.min(*y2),
                        (x2 - x1).abs().max(width),
                        (y2 - y1).abs().max(width),
                    ),
                    clip,
                    color,
                    radius: width / 2.0,
                })?,
                (4, [x, y, radius]) if *radius >= 0.0 => self.push(DrawCommand::FillRect {
                    node,
                    rect: local_rect(*x - *radius, *y - *radius, *radius * 2.0, *radius * 2.0),
                    clip,
                    color,
                    radius: *radius,
                })?,
                (5, [x, y, size]) if *size > 0.0 => {
                    self.text_bytes = self
                        .text_bytes
                        .checked_add(payload.len())
                        .ok_or(PaintError::TextLimitExceeded)?;
                    if self.text_bytes > self.limits.max_text_bytes {
                        return Err(PaintError::TextLimitExceeded);
                    }
                    self.push(DrawCommand::Text {
                        node,
                        rect: local_rect(*x, *y - *size, bounds.width.max(0.0), *size * 1.4),
                        clip,
                        color,
                        font_size: *size,
                        font_weight: 400,
                        value: payload.into(),
                    })?;
                }
                (6, values)
                    if !values.is_empty()
                        && payload
                            .bytes()
                            .all(|value| matches!(value, b'M' | b'L' | b'Q' | b'Z')) =>
                {
                    let mut minimum_x = f64::INFINITY;
                    let mut minimum_y = f64::INFINITY;
                    let mut maximum_x = f64::NEG_INFINITY;
                    let mut maximum_y = f64::NEG_INFINITY;
                    for pair in values.chunks_exact(2) {
                        minimum_x = minimum_x.min(pair[0]);
                        minimum_y = minimum_y.min(pair[1]);
                        maximum_x = maximum_x.max(pair[0]);
                        maximum_y = maximum_y.max(pair[1]);
                    }
                    self.push(DrawCommand::FillRect {
                        node,
                        rect: local_rect(
                            minimum_x,
                            minimum_y,
                            (maximum_x - minimum_x).max(width),
                            (maximum_y - minimum_y).max(width),
                        ),
                        clip,
                        color,
                        radius: width / 2.0,
                    })?;
                }
                _ => return Err(PaintError::InvalidGraphicsProgram(node)),
            }
        }
        Ok(())
    }

    fn push(&mut self, command: DrawCommand) -> Result<(), PaintError> {
        if self.commands.len() >= self.limits.max_commands {
            return Err(PaintError::CommandLimitExceeded);
        }
        self.commands.push(command);
        Ok(())
    }

    fn push_scrollbars(&mut self) -> Result<(), PaintError> {
        let metrics = self.layout.scroll_iter().copied().collect::<Vec<_>>();
        for metrics in metrics {
            let thickness = 6.0_f64;
            if metrics.max_offset_y > 0.0 && metrics.viewport.height > 0.0 {
                let track = Rect::new(
                    (metrics.viewport.x + metrics.viewport.width - thickness)
                        .max(metrics.viewport.x),
                    metrics.viewport.y,
                    thickness.min(metrics.viewport.width),
                    metrics.viewport.height,
                );
                self.push(DrawCommand::Scrollbar {
                    node: metrics.node,
                    track,
                    thumb: scrollbar_thumb(
                        track,
                        metrics.viewport.height,
                        metrics.max_offset_y,
                        metrics.offset_y,
                        false,
                    ),
                    color: 0x66000000,
                })?;
            }
            if metrics.max_offset_x > 0.0 && metrics.viewport.width > 0.0 {
                let track = Rect::new(
                    metrics.viewport.x,
                    (metrics.viewport.y + metrics.viewport.height - thickness)
                        .max(metrics.viewport.y),
                    metrics.viewport.width,
                    thickness.min(metrics.viewport.height),
                );
                self.push(DrawCommand::Scrollbar {
                    node: metrics.node,
                    track,
                    thumb: scrollbar_thumb(
                        track,
                        metrics.viewport.width,
                        metrics.max_offset_x,
                        metrics.offset_x,
                        true,
                    ),
                    color: 0x66000000,
                })?;
            }
        }
        Ok(())
    }

    fn order_overlay_planes(&mut self) -> Result<(), PaintError> {
        let mut planes = BTreeMap::new();
        let mut pending = Vec::new();
        pending.push((self.tree.root(), None));
        while let Some((id, inherited_plane)) = pending.pop() {
            let node = self.tree.node(id).ok_or(PaintError::MissingNode(id))?;
            let plane = integer_property(&node, PropertyId::PORTAL_LAYER, id)?
                .map(Some)
                .unwrap_or(inherited_plane);
            planes.insert(id, plane);
            for child in node.children.iter().rev() {
                pending.push((*child, plane));
            }
        }
        self.commands.sort_by_key(|command| {
            planes
                .get(&draw_command_node(command))
                .copied()
                .flatten()
                .map_or((0_u8, 0_i64), |layer| (1_u8, layer))
        });
        Ok(())
    }
}

const fn draw_command_node(command: &DrawCommand) -> NodeId {
    match command {
        DrawCommand::FillRect { node, .. }
        | DrawCommand::StrokeRect { node, .. }
        | DrawCommand::Text { node, .. }
        | DrawCommand::TextEditor { node, .. }
        | DrawCommand::Scrollbar { node, .. } => *node,
    }
}

fn scrollbar_thumb(
    track: Rect,
    viewport_extent: f64,
    max_offset: f64,
    offset: f64,
    horizontal: bool,
) -> Rect {
    let track_extent = if horizontal {
        track.width
    } else {
        track.height
    };
    let content_extent = viewport_extent + max_offset;
    let thumb_extent = (track_extent * viewport_extent / content_extent)
        .max(16.0_f64.min(track_extent))
        .min(track_extent);
    let travel = (track_extent - thumb_extent).max(0.0);
    let position = if max_offset > 0.0 {
        offset.clamp(0.0, max_offset) / max_offset * travel
    } else {
        0.0
    };
    if horizontal {
        Rect::new(track.x + position, track.y, thumb_extent, track.height)
    } else {
        Rect::new(track.x, track.y + position, track.width, thumb_extent)
    }
}

fn color_with_opacity(color: u32, opacity: f64) -> u32 {
    let alpha = ((color >> 24) as f64 * opacity + 0.5) as u32;
    (color & 0x00ff_ffff) | (alpha << 24)
}

fn color_property(
    node: &NodeSnapshot,
    property: PropertyId,
    id: NodeId,
) -> Result<Option<u32>, PaintError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    match value {
        Value::Color(value) => Ok(Some(*value)),
        _ => Err(PaintError::InvalidProperty(id, property)),
    }
}

fn boolean_property(
    node: &NodeSnapshot,
    property: PropertyId,
    id: NodeId,
) -> Result<Option<bool>, PaintError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    match value {
        Value::Bool(value) => Ok(Some(*value)),
        _ => Err(PaintError::InvalidProperty(id, property)),
    }
}

fn number_property(
    node: &NodeSnapshot,
    property: PropertyId,
    id: NodeId,
) -> Result<Option<f64>, PaintError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    let value = match value {
        Value::I64(value) => *value as f64,
        Value::F64(value) => *value,
        Value::Length(Length::Px(value)) => f64::from(*value),
        _ => return Err(PaintError::InvalidProperty(id, property)),
    };
    if !value.is_finite() || value < 0.0 {
        return Err(PaintError::InvalidProperty(id, property));
    }
    Ok(Some(value))
}

fn integer_property(
    node: &NodeSnapshot,
    property: PropertyId,
    id: NodeId,
) -> Result<Option<i64>, PaintError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    match value {
        Value::I64(value) => Ok(Some(*value)),
        _ => Err(PaintError::InvalidProperty(id, property)),
    }
}

fn unsigned_property(
    node: &NodeSnapshot,
    property: PropertyId,
    id: NodeId,
) -> Result<Option<u32>, PaintError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    match value {
        Value::I64(value) if *value >= 0 => u32::try_from(*value)
            .map(Some)
            .map_err(|_| PaintError::InvalidProperty(id, property)),
        _ => Err(PaintError::InvalidProperty(id, property)),
    }
}

fn string_property(
    node: &NodeSnapshot,
    property: PropertyId,
    id: NodeId,
) -> Result<Option<String>, PaintError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    match value {
        Value::Text(value) => Ok(Some(value.clone())),
        _ => Err(PaintError::InvalidProperty(id, property)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_ui_core::{Length, Primitive, Property};
    use vo_ui_layout::{compute_layout, ApproximateTextMeasurer, LayoutLimits};
    use vo_ui_protocol::{Mutation, MutationBatch, ProtocolLimits};

    fn fixture() -> (TreeMirror, LayoutSnapshot) {
        let root = NodeId::new(0, 1);
        let card = NodeId::new(1, 1);
        let text = NodeId::new(2, 1);
        let mut tree = TreeMirror::new(7, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            7,
            1,
            alloc::vec![
                Mutation::Create {
                    id: card,
                    kind: NodeKind::Element(Primitive::Box)
                },
                Mutation::SetProperty {
                    id: card,
                    property: Property::new(PropertyId::BACKGROUND, Value::Color(0xff112233))
                },
                Mutation::SetProperty {
                    id: card,
                    property: Property::new(PropertyId::FOREGROUND, Value::Color(0xffabcdef))
                },
                Mutation::SetProperty {
                    id: card,
                    property: Property::new(PropertyId::RADIUS, Value::Length(Length::Px(6.0)))
                },
                Mutation::SetProperty {
                    id: card,
                    property: Property::new(PropertyId::BORDER_COLOR, Value::Color(0xff526680))
                },
                Mutation::SetProperty {
                    id: card,
                    property: Property::new(
                        PropertyId::BORDER_WIDTH,
                        Value::Length(Length::Px(1.5))
                    )
                },
                Mutation::Create {
                    id: text,
                    kind: NodeKind::Text
                },
                Mutation::SetText {
                    id: text,
                    text: "hello".into()
                },
                Mutation::InsertBefore {
                    parent: card,
                    child: text,
                    before: None
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: card,
                    before: None
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        (tree, layout)
    }

    #[test]
    fn scene_order_and_inherited_text_style_are_deterministic() {
        let (tree, layout) = fixture();
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        assert_eq!(scene.revision, 1);
        assert_eq!(scene.len(), 3);
        assert!(matches!(
            scene.commands()[0],
            DrawCommand::FillRect {
                color: 0xff112233,
                radius: 6.0,
                ..
            }
        ));
        assert!(matches!(
            scene.commands()[1],
            DrawCommand::StrokeRect {
                color: 0xff526680,
                radius: 6.0,
                width: 1.5,
                ..
            }
        ));
        assert!(matches!(
            &scene.commands()[2],
            DrawCommand::Text {
                color: 0xffabcdef,
                value,
                ..
            } if value == "hello"
        ));
    }

    #[test]
    fn opacity_multiplies_container_and_descendant_paint_alpha() {
        let (mut tree, _) = fixture();
        tree.apply(&MutationBatch::new(
            7,
            2,
            alloc::vec![Mutation::SetProperty {
                id: NodeId::new(1, 1),
                property: Property::new(PropertyId::OPACITY, Value::F64(0.5)),
            }],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        assert!(matches!(
            scene.commands()[0],
            DrawCommand::FillRect {
                color: 0x80112233,
                ..
            }
        ));
        assert!(matches!(
            scene.commands()[1],
            DrawCommand::StrokeRect {
                color: 0x80526680,
                ..
            }
        ));
        assert!(matches!(
            scene.commands()[2],
            DrawCommand::Text {
                color: 0x80abcdef,
                ..
            }
        ));
    }

    #[test]
    fn invalid_border_width_fails_closed() {
        let (mut tree, _) = fixture();
        tree.apply(&MutationBatch::new(
            7,
            2,
            alloc::vec![Mutation::SetProperty {
                id: NodeId::new(1, 1),
                property: Property::new(PropertyId::BORDER_WIDTH, Value::F64(-1.0)),
            }],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(
            build_paint_scene(&tree, &layout, PaintLimits::default()),
            Err(PaintError::InvalidProperty(
                NodeId::new(1, 1),
                PropertyId::BORDER_WIDTH
            ))
        );
    }

    #[test]
    fn slider_paints_track_progress_and_thumb_from_numeric_range() {
        let root = NodeId::new(0, 1);
        let slider = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(8, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            8,
            1,
            alloc::vec![
                Mutation::Create {
                    id: slider,
                    kind: NodeKind::Element(Primitive::Slider),
                },
                Mutation::SetProperty {
                    id: slider,
                    property: Property::new(PropertyId::WIDTH, Value::Length(Length::Px(200.0))),
                },
                Mutation::SetProperty {
                    id: slider,
                    property: Property::new(PropertyId::HEIGHT, Value::Length(Length::Px(24.0))),
                },
                Mutation::SetProperty {
                    id: slider,
                    property: Property::new(PropertyId::BACKGROUND, Value::Color(0xff64748b)),
                },
                Mutation::SetProperty {
                    id: slider,
                    property: Property::new(PropertyId::FOREGROUND, Value::Color(0xff4f7cff)),
                },
                Mutation::SetProperty {
                    id: slider,
                    property: Property::new(PropertyId::VALUE, 25.0_f64),
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
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        assert_eq!(scene.commands().len(), 3);
        assert!(matches!(
            scene.commands()[0],
            DrawCommand::FillRect {
                color: 0xff64748b,
                rect,
                ..
            } if rect.width == 200.0 && rect.height == 4.0
        ));
        assert!(matches!(
            scene.commands()[1],
            DrawCommand::FillRect {
                color: 0xff4f7cff,
                rect,
                ..
            } if rect.width == 50.0 && rect.height == 4.0
        ));
        assert!(matches!(
            scene.commands()[2],
            DrawCommand::FillRect {
                color: 0xff4f7cff,
                rect,
                radius: 8.0,
                ..
            } if rect.width == 16.0 && rect.height == 16.0
        ));
    }

    #[test]
    fn bounded_vgc1_canvas_program_lowers_into_native_draw_commands() {
        let root = NodeId::new(0, 1);
        let canvas = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(7, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            7,
            1,
            alloc::vec![
                Mutation::Create { id: canvas, kind: NodeKind::Element(Primitive::Canvas) },
                Mutation::SetProperty { id: canvas, property: Property::new(PropertyId::WIDTH, Value::Length(Length::Px(200.0))) },
                Mutation::SetProperty { id: canvas, property: Property::new(PropertyId::HEIGHT, Value::Length(Length::Px(100.0))) },
                Mutation::SetProperty {
                    id: canvas,
                    property: Property::new(
                        PropertyId::GRAPHICS_PROGRAM,
                        "VGC1\n0||ff101820|0|\n1|8,9,20,30|ff4f7cff|0|\n5|10,50,16|ffffffff|0|hello",
                    ),
                },
                Mutation::InsertBefore { parent: root, child: canvas, before: None },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        assert_eq!(scene.commands().len(), 3);
        assert!(matches!(
            scene.commands()[1],
            DrawCommand::FillRect {
                color: 0xff4f7cff,
                ..
            }
        ));
        assert!(
            matches!(&scene.commands()[2], DrawCommand::Text { value, .. } if value == "hello")
        );

        tree.apply(&MutationBatch::new(
            7,
            2,
            alloc::vec![Mutation::SetProperty {
                id: canvas,
                property: Property::new(
                    PropertyId::GRAPHICS_PROGRAM,
                    "VGC1\n5|10,50,NaN|ffffffff|0|untrusted",
                ),
            }],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert!(matches!(
            build_paint_scene(&tree, &layout, PaintLimits::default()),
            Err(PaintError::InvalidGraphicsProgram(id)) if id == canvas
        ));
    }

    #[test]
    fn portal_planes_paint_after_content_in_stable_layer_order() {
        let root = NodeId::new(0, 1);
        let high = NodeId::new(1, 1);
        let content = NodeId::new(2, 1);
        let low = NodeId::new(3, 1);
        let mut tree = TreeMirror::new(7, root, ProtocolLimits::default());
        let mut mutations = Vec::new();
        for (id, layer, color) in [
            (high, Some(20_i64), 0xff000014_u32),
            (content, None, 0xff000000_u32),
            (low, Some(10_i64), 0xff00000a_u32),
        ] {
            mutations.push(Mutation::Create {
                id,
                kind: NodeKind::Element(Primitive::Box),
            });
            mutations.push(Mutation::SetProperty {
                id,
                property: Property::new(PropertyId::BACKGROUND, Value::Color(color)),
            });
            if let Some(layer) = layer {
                mutations.push(Mutation::SetProperty {
                    id,
                    property: Property::new(PropertyId::PORTAL_LAYER, Value::I64(layer)),
                });
            }
            mutations.push(Mutation::InsertBefore {
                parent: root,
                child: id,
                before: None,
            });
        }
        tree.apply(&MutationBatch::new(7, 1, mutations)).unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        let order = scene
            .commands()
            .iter()
            .map(draw_command_node)
            .collect::<Vec<_>>();
        assert_eq!(order, alloc::vec![content, low, high]);
    }

    #[test]
    fn hidden_subtree_is_not_painted_or_required_in_layout() {
        let (mut tree, _) = fixture();
        let card = NodeId::new(1, 1);
        tree.apply(&MutationBatch::new(
            7,
            2,
            alloc::vec![Mutation::SetProperty {
                id: card,
                property: Property::new(PropertyId::HIDDEN, Value::Bool(true)),
            }],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert!(layout.get(card).is_none());

        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        assert!(scene.is_empty());
    }

    #[test]
    fn revision_and_command_limits_fail_closed() {
        let (mut tree, layout) = fixture();
        tree.apply(&MutationBatch::new(7, 2, Vec::new())).unwrap();
        assert_eq!(
            build_paint_scene(&tree, &layout, PaintLimits::default()),
            Err(PaintError::RevisionMismatch)
        );
        let (tree, layout) = fixture();
        assert_eq!(
            build_paint_scene(
                &tree,
                &layout,
                PaintLimits {
                    max_commands: 1,
                    ..PaintLimits::default()
                }
            ),
            Err(PaintError::CommandLimitExceeded)
        );
    }
}
