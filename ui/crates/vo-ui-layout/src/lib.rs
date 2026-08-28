#![no_std]

extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{Length, NodeId, Primitive, PropertyId, Value};
use vo_ui_protocol::{NodeKind, NodeSnapshot, TreeMirror};

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Size {
    pub width: f64,
    pub height: f64,
}

impl Size {
    pub const fn new(width: f64, height: f64) -> Self {
        Self { width, height }
    }

    fn is_valid(self) -> bool {
        self.width.is_finite() && self.height.is_finite() && self.width >= 0.0 && self.height >= 0.0
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct Rect {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
}

impl Rect {
    pub const fn new(x: f64, y: f64, width: f64, height: f64) -> Self {
        Self {
            x,
            y,
            width,
            height,
        }
    }

    pub fn contains(self, x: f64, y: f64) -> bool {
        x >= self.x && y >= self.y && x <= self.x + self.width && y <= self.y + self.height
    }

    pub fn intersection(self, other: Self) -> Self {
        let x = self.x.max(other.x);
        let y = self.y.max(other.y);
        let right = (self.x + self.width).min(other.x + other.width);
        let bottom = (self.y + self.height).min(other.y + other.height);
        Self::new(x, y, (right - x).max(0.0), (bottom - y).max(0.0))
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LayoutBox {
    pub node: NodeId,
    pub rect: Rect,
    pub content: Rect,
    /// Effective ancestor clip in viewport coordinates. Hosts use the same
    /// rectangle for painting, accessibility visibility, and hit testing.
    pub clip: Option<Rect>,
    pub z_index: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LayoutSnapshot {
    pub revision: u64,
    pub viewport: Size,
    boxes: BTreeMap<NodeId, LayoutBox>,
    scroll: BTreeMap<NodeId, ScrollMetrics>,
}

impl LayoutSnapshot {
    pub fn get(&self, node: NodeId) -> Option<&LayoutBox> {
        self.boxes.get(&node)
    }

    pub fn iter(&self) -> impl Iterator<Item = &LayoutBox> {
        self.boxes.values()
    }

    pub fn len(&self) -> usize {
        self.boxes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.boxes.is_empty()
    }

    pub fn scroll(&self, node: NodeId) -> Option<&ScrollMetrics> {
        self.scroll.get(&node)
    }

    pub fn scroll_iter(&self) -> impl Iterator<Item = &ScrollMetrics> {
        self.scroll.values()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ScrollMetrics {
    pub node: NodeId,
    pub viewport: Rect,
    pub offset_x: f64,
    pub offset_y: f64,
    pub max_offset_x: f64,
    pub max_offset_y: f64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LayoutLimits {
    pub max_nodes: usize,
    pub max_depth: usize,
    pub max_grid_tracks: usize,
    pub max_grid_area_cells: usize,
}

impl Default for LayoutLimits {
    fn default() -> Self {
        Self {
            max_nodes: 100_000,
            max_depth: 1_024,
            max_grid_tracks: 256,
            max_grid_area_cells: 65_536,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LayoutError {
    InvalidViewport,
    InvalidLimits,
    MissingNode(NodeId),
    InvalidNumber(NodeId, PropertyId),
    InvalidGridTracks(NodeId),
    GridTrackLimitExceeded(NodeId),
    InvalidGridAreas(NodeId),
    InvalidScrollOffset(NodeId),
    GridAreaLimitExceeded(NodeId),
    NodeLimitExceeded,
    DepthLimitExceeded,
}

impl fmt::Display for LayoutError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "UI layout failed: {self:?}")
    }
}

pub trait IntrinsicMeasurer {
    fn measure_text(&mut self, node: NodeId, text: &str, font_size: f64, max_width: f64) -> Size;

    fn measure_replaced(&mut self, _node: NodeId, primitive: Primitive, _available: Size) -> Size {
        match primitive {
            Primitive::TextInput => Size::new(160.0, 32.0),
            Primitive::TextArea => Size::new(320.0, 180.0),
            Primitive::Toggle => Size::new(40.0, 24.0),
            Primitive::Slider => Size::new(160.0, 24.0),
            Primitive::Image | Primitive::Canvas | Primitive::PlatformView => Size::new(0.0, 0.0),
            _ => Size::new(0.0, 0.0),
        }
    }
}

pub trait ScrollOffsetProvider {
    fn resolve_scroll_offset(
        &mut self,
        _node: NodeId,
        declared_x: f64,
        declared_y: f64,
    ) -> (f64, f64) {
        (declared_x, declared_y)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct DeclaredScrollOffsets;

impl ScrollOffsetProvider for DeclaredScrollOffsets {}

/// Deterministic fallback used by tests and bootstrap hosts. Production text
/// backends supply a shaper-backed measurer while retaining the same layout
/// traversal and constraints.
#[derive(Clone, Copy, Debug, Default)]
pub struct ApproximateTextMeasurer;

impl IntrinsicMeasurer for ApproximateTextMeasurer {
    fn measure_text(&mut self, _node: NodeId, text: &str, font_size: f64, max_width: f64) -> Size {
        let glyphs = text.chars().count() as f64;
        let raw_width = glyphs * font_size * 0.55;
        if max_width <= 0.0 || raw_width <= max_width {
            return Size::new(raw_width, font_size * 1.25);
        }
        let ratio = raw_width / max_width;
        let whole_lines = ratio as u64;
        let lines = if whole_lines as f64 >= ratio {
            whole_lines
        } else {
            whole_lines.saturating_add(1)
        }
        .max(1) as f64;
        Size::new(max_width, lines * font_size * 1.25)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Flow {
    Row,
    Column,
    Grid,
    Stack,
    Leaf,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum GridTrack {
    Px(f64),
    Fraction(f64),
    Auto,
    MinMax { min: GridBreadth, max: GridBreadth },
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum GridBreadth {
    Px(f64),
    Fraction(f64),
    Auto,
}

const MAX_GRID_SYNTAX_DEPTH: usize = 16;

struct MeasuredNode {
    id: NodeId,
    size: Size,
    padding: f64,
    gap: f64,
    flex: f64,
    flow: Flow,
    grid_tracks: Vec<GridTrack>,
    grid_areas: Option<GridAreaTemplate>,
    grid_area: Option<String>,
    clips_children: bool,
    scroll_x: f64,
    scroll_y: f64,
    rtl: bool,
    portal_layer: Option<i64>,
    children: Vec<MeasuredNode>,
}

struct LayoutContext<'a, M> {
    tree: &'a TreeMirror,
    viewport: Size,
    limits: LayoutLimits,
    measured_nodes: usize,
    measurer: &'a mut M,
    scroll_offsets: &'a mut dyn ScrollOffsetProvider,
    resolved_scroll_offsets: BTreeMap<NodeId, (f64, f64)>,
}

pub fn compute_layout<M: IntrinsicMeasurer>(
    tree: &TreeMirror,
    viewport: Size,
    limits: LayoutLimits,
    measurer: &mut M,
) -> Result<LayoutSnapshot, LayoutError> {
    compute_layout_with_scroll_offsets(tree, viewport, limits, measurer, &mut DeclaredScrollOffsets)
}

pub fn compute_layout_with_scroll_offsets<M: IntrinsicMeasurer>(
    tree: &TreeMirror,
    viewport: Size,
    limits: LayoutLimits,
    measurer: &mut M,
    scroll_offsets: &mut dyn ScrollOffsetProvider,
) -> Result<LayoutSnapshot, LayoutError> {
    if !viewport.is_valid() {
        return Err(LayoutError::InvalidViewport);
    }
    if limits.max_nodes == 0
        || limits.max_depth == 0
        || limits.max_grid_tracks == 0
        || limits.max_grid_area_cells == 0
    {
        return Err(LayoutError::InvalidLimits);
    }
    let mut context = LayoutContext {
        tree,
        viewport,
        limits,
        measured_nodes: 0,
        measurer,
        scroll_offsets,
        resolved_scroll_offsets: BTreeMap::new(),
    };
    let mut root = context.measure(tree.root(), viewport, 0, 14.0, false)?;
    root.size = viewport;
    let mut boxes = BTreeMap::new();
    let portal_bounds = Rect::new(0.0, 0.0, viewport.width, viewport.height);
    place(
        &root,
        Rect::new(0.0, 0.0, root.size.width, root.size.height),
        0,
        0,
        None,
        portal_bounds,
        &mut boxes,
    );
    let scroll = build_scroll_metrics(tree, &boxes, &context.resolved_scroll_offsets);
    Ok(LayoutSnapshot {
        revision: tree.revision(),
        viewport,
        boxes,
        scroll,
    })
}

impl<M: IntrinsicMeasurer> LayoutContext<'_, M> {
    fn measure(
        &mut self,
        id: NodeId,
        available: Size,
        depth: usize,
        inherited_font_size: f64,
        inherited_rtl: bool,
    ) -> Result<MeasuredNode, LayoutError> {
        if depth >= self.limits.max_depth {
            return Err(LayoutError::DepthLimitExceeded);
        }
        self.measured_nodes += 1;
        if self.measured_nodes > self.limits.max_nodes {
            return Err(LayoutError::NodeLimitExceeded);
        }
        let node = self.tree.node(id).ok_or(LayoutError::MissingNode(id))?;
        let font_size = number_property(&node, PropertyId::FONT_SIZE, id)?
            .unwrap_or(inherited_font_size)
            .max(0.0);
        let rtl = match node.properties.get(&PropertyId::FLOW_DIRECTION) {
            None => inherited_rtl,
            Some(Value::I64(0)) => false,
            Some(Value::I64(1)) => true,
            Some(_) => return Err(LayoutError::InvalidNumber(id, PropertyId::FLOW_DIRECTION)),
        };
        let portal_layer = match node.properties.get(&PropertyId::PORTAL_LAYER) {
            None => None,
            Some(Value::I64(layer @ -1_000_000..=1_000_000)) => Some(*layer),
            Some(_) => return Err(LayoutError::InvalidNumber(id, PropertyId::PORTAL_LAYER)),
        };
        let padding = length_property(
            &node,
            PropertyId::PADDING,
            available.width,
            self.viewport,
            id,
        )?
        .unwrap_or_else(|| default_padding(&node));
        let gap = length_property(&node, PropertyId::GAP, available.width, self.viewport, id)?
            .unwrap_or(0.0);
        let flex = number_property(&node, PropertyId::FLEX, id)?
            .unwrap_or(0.0)
            .max(0.0);
        let declared_scroll_x = number_property(&node, PropertyId::SCROLL_X, id)?
            .unwrap_or(0.0)
            .max(0.0);
        let declared_scroll_y = number_property(&node, PropertyId::SCROLL_Y, id)?
            .unwrap_or(0.0)
            .max(0.0);
        let (scroll_x, scroll_y) =
            self.scroll_offsets
                .resolve_scroll_offset(id, declared_scroll_x, declared_scroll_y);
        if !scroll_x.is_finite() || !scroll_y.is_finite() {
            return Err(LayoutError::InvalidScrollOffset(id));
        }
        self.resolved_scroll_offsets
            .insert(id, (scroll_x, scroll_y));
        let explicit_width =
            length_property(&node, PropertyId::WIDTH, available.width, self.viewport, id)?;
        let explicit_height = length_property(
            &node,
            PropertyId::HEIGHT,
            available.height,
            self.viewport,
            id,
        )?;
        let flow = flow(&node);
        let grid_area = grid_area_name(&node, id)?;
        let grid_areas = if flow == Flow::Grid {
            grid_template_areas(&node, id, self.limits)?
        } else {
            None
        };
        let grid_tracks = if flow == Flow::Grid {
            grid_tracks(
                &node,
                id,
                self.limits.max_grid_tracks,
                grid_areas.as_ref().map(GridAreaTemplate::column_count),
            )?
        } else {
            Vec::new()
        };
        if grid_areas
            .as_ref()
            .is_some_and(|areas| areas.column_count() != grid_tracks.len())
        {
            return Err(LayoutError::InvalidGridAreas(id));
        }
        let clips_children = clips_children(&node);
        let content_available = Size::new(
            (explicit_width.unwrap_or(available.width) - padding * 2.0).max(0.0),
            (explicit_height.unwrap_or(available.height) - padding * 2.0).max(0.0),
        );
        let mut children = Vec::with_capacity(node.children.len());
        for child in &node.children {
            let child_node = self
                .tree
                .node(*child)
                .ok_or(LayoutError::MissingNode(*child))?;
            match child_node.properties.get(&PropertyId::HIDDEN) {
                Some(Value::Bool(true)) => continue,
                Some(Value::Bool(false)) | None => {}
                Some(_) => return Err(LayoutError::InvalidNumber(*child, PropertyId::HIDDEN)),
            }
            let child_available = self
                .tree
                .node(*child)
                .filter(|child| child.properties.contains_key(&PropertyId::PORTAL_LAYER))
                .map_or(content_available, |_| self.viewport);
            children.push(self.measure(*child, child_available, depth + 1, font_size, rtl)?);
        }
        if let Some(areas) = &grid_areas {
            for child in &children {
                if child
                    .grid_area
                    .as_ref()
                    .is_some_and(|name| !areas.areas.contains_key(name))
                {
                    return Err(LayoutError::InvalidGridAreas(id));
                }
            }
        }

        let intrinsic = match (&node.kind, flow) {
            (NodeKind::Text, _) => {
                self.measurer
                    .measure_text(id, &node.text, font_size, content_available.width)
            }
            (NodeKind::Element(primitive), Flow::Leaf) => {
                self.measurer
                    .measure_replaced(id, *primitive, content_available)
            }
            (_, Flow::Row) => Size::new(sum_main(&children, gap, true), max_cross(&children, true)),
            (_, Flow::Column) => {
                Size::new(max_cross(&children, false), sum_main(&children, gap, false))
            }
            (_, Flow::Grid) => {
                let metrics = grid_metrics(
                    &children,
                    &grid_tracks,
                    grid_areas.as_ref(),
                    content_available.width,
                    gap,
                );
                Size::new(metrics.width, metrics.height)
            }
            (_, Flow::Stack) => Size::new(
                children
                    .iter()
                    .filter(|child| child.portal_layer.is_none())
                    .fold(0.0_f64, |value, child| value.max(child.size.width)),
                children
                    .iter()
                    .filter(|child| child.portal_layer.is_none())
                    .fold(0.0_f64, |value, child| value.max(child.size.height)),
            ),
        };
        let stretch_width = matches!(
            node.kind,
            NodeKind::Element(
                Primitive::Root
                    | Primitive::Box
                    | Primitive::Row
                    | Primitive::Column
                    | Primitive::Stack
                    | Primitive::Grid
                    | Primitive::Scroll
            )
        );
        let mut size = Size::new(
            explicit_width.unwrap_or({
                if stretch_width {
                    available.width
                } else {
                    intrinsic.width + padding * 2.0
                }
            }),
            explicit_height.unwrap_or(intrinsic.height + padding * 2.0),
        );
        constrain_size(&node, id, available, self.viewport, &mut size)?;
        if !size.is_valid() {
            return Err(LayoutError::InvalidNumber(id, PropertyId::WIDTH));
        }
        Ok(MeasuredNode {
            id,
            size,
            padding,
            gap,
            flex,
            flow,
            grid_tracks,
            grid_areas,
            grid_area,
            clips_children,
            scroll_x,
            scroll_y,
            rtl,
            portal_layer,
            children,
        })
    }
}

fn place(
    node: &MeasuredNode,
    assigned: Rect,
    depth: i32,
    stack_base: i32,
    inherited_clip: Option<Rect>,
    portal_bounds: Rect,
    boxes: &mut BTreeMap<NodeId, LayoutBox>,
) {
    let stack_base = node.portal_layer.map_or(stack_base, |layer| {
        i32::try_from(layer.saturating_mul(1_000)).unwrap_or(if layer < 0 {
            i32::MIN
        } else {
            i32::MAX
        })
    });
    let content = Rect::new(
        assigned.x + node.padding,
        assigned.y + node.padding,
        (assigned.width - node.padding * 2.0).max(0.0),
        (assigned.height - node.padding * 2.0).max(0.0),
    );
    boxes.insert(
        node.id,
        LayoutBox {
            node: node.id,
            rect: assigned,
            content,
            clip: inherited_clip,
            z_index: stack_base.saturating_add(depth),
        },
    );
    if node.children.is_empty() {
        return;
    }
    let child_clip = if node.clips_children {
        Some(match inherited_clip {
            Some(clip) => clip.intersection(content),
            None => content,
        })
    } else {
        inherited_clip
    };
    if node.flow == Flow::Grid {
        place_grid(
            node,
            content,
            depth,
            stack_base,
            child_clip,
            portal_bounds,
            boxes,
        );
        return;
    }
    let horizontal = node.flow == Flow::Row;
    let flowing = matches!(node.flow, Flow::Row | Flow::Column);
    let used = if flowing {
        sum_main(&node.children, node.gap, horizontal)
    } else {
        0.0
    };
    let available_main = if horizontal {
        content.width
    } else {
        content.height
    };
    let total_flex = node
        .children
        .iter()
        .filter(|child| child.portal_layer.is_none())
        .map(|child| child.flex)
        .sum::<f64>();
    let free = (available_main - used).max(0.0);
    let mut cursor = if horizontal && node.rtl {
        content.x + content.width + node.scroll_x
    } else if horizontal {
        content.x - node.scroll_x
    } else {
        content.y - node.scroll_y
    };
    for child in &node.children {
        if child.portal_layer.is_some() {
            place(
                child,
                portal_bounds,
                depth + 1,
                stack_base,
                None,
                portal_bounds,
                boxes,
            );
            continue;
        }
        let mut child_size = child.size;
        if flowing && child.flex > 0.0 && total_flex > 0.0 {
            let addition = free * child.flex / total_flex;
            if horizontal {
                child_size.width += addition;
            } else {
                child_size.height += addition;
            }
        }
        if flowing {
            if horizontal {
                child_size.height = child_size.height.min(content.height);
                if node.rtl {
                    cursor -= child_size.width;
                    place(
                        child,
                        Rect::new(
                            cursor,
                            content.y - node.scroll_y,
                            child_size.width,
                            child_size.height,
                        ),
                        depth + 1,
                        stack_base,
                        child_clip,
                        portal_bounds,
                        boxes,
                    );
                    cursor -= node.gap;
                } else {
                    place(
                        child,
                        Rect::new(
                            cursor,
                            content.y - node.scroll_y,
                            child_size.width,
                            child_size.height,
                        ),
                        depth + 1,
                        stack_base,
                        child_clip,
                        portal_bounds,
                        boxes,
                    );
                    cursor += child_size.width + node.gap;
                }
            } else {
                child_size.width = child_size.width.min(content.width);
                place(
                    child,
                    Rect::new(
                        content.x - node.scroll_x,
                        cursor,
                        child_size.width,
                        child_size.height,
                    ),
                    depth + 1,
                    stack_base,
                    child_clip,
                    portal_bounds,
                    boxes,
                );
                cursor += child_size.height + node.gap;
            }
        } else {
            place(
                child,
                Rect::new(
                    content.x - node.scroll_x,
                    content.y - node.scroll_y,
                    child_size.width,
                    child_size.height,
                ),
                depth + 1,
                stack_base,
                child_clip,
                portal_bounds,
                boxes,
            );
        }
    }
}

fn place_grid(
    node: &MeasuredNode,
    content: Rect,
    depth: i32,
    stack_base: i32,
    child_clip: Option<Rect>,
    portal_bounds: Rect,
    boxes: &mut BTreeMap<NodeId, LayoutBox>,
) {
    let metrics = grid_metrics(
        &node.children,
        &node.grid_tracks,
        node.grid_areas.as_ref(),
        content.width,
        node.gap,
    );
    let mut column_offsets = Vec::with_capacity(metrics.columns.len());
    let mut cursor = content.x - node.scroll_x;
    for width in &metrics.columns {
        column_offsets.push(cursor);
        cursor += width + node.gap;
    }
    let mut row_offsets = Vec::with_capacity(metrics.rows.len());
    cursor = content.y - node.scroll_y;
    for height in &metrics.rows {
        row_offsets.push(cursor);
        cursor += height + node.gap;
    }
    for (child, placement) in node.children.iter().zip(&metrics.placements) {
        let Some(placement) = placement else {
            place(
                child,
                portal_bounds,
                depth + 1,
                stack_base,
                None,
                portal_bounds,
                boxes,
            );
            continue;
        };
        let width = metrics.columns[placement.column..placement.column + placement.column_span]
            .iter()
            .sum::<f64>()
            + node.gap * placement.column_span.saturating_sub(1) as f64;
        let height = metrics.rows[placement.row..placement.row + placement.row_span]
            .iter()
            .sum::<f64>()
            + node.gap * placement.row_span.saturating_sub(1) as f64;
        place(
            child,
            Rect::new(
                column_offsets[placement.column],
                row_offsets[placement.row],
                width,
                height,
            ),
            depth + 1,
            stack_base,
            child_clip,
            portal_bounds,
            boxes,
        );
    }
}

fn flow(node: &NodeSnapshot) -> Flow {
    match node.kind {
        NodeKind::Text => Flow::Leaf,
        NodeKind::Element(Primitive::Row) => Flow::Row,
        NodeKind::Element(Primitive::Stack) => Flow::Stack,
        NodeKind::Element(Primitive::Grid) => Flow::Grid,
        NodeKind::Element(
            Primitive::Root
            | Primitive::Fragment
            | Primitive::Box
            | Primitive::Column
            | Primitive::Scroll
            | Primitive::Button
            | Primitive::Text,
        ) => Flow::Column,
        NodeKind::Element(_) => Flow::Leaf,
    }
}

fn clips_children(node: &NodeSnapshot) -> bool {
    if matches!(node.kind, NodeKind::Element(Primitive::Scroll)) {
        return true;
    }
    matches!(
        node.properties.get(&PropertyId::OVERFLOW),
        Some(Value::Text(value))
            if matches!(value.as_str(), "hidden" | "clip" | "scroll" | "auto")
    )
}

fn scroll_container(node: &NodeSnapshot) -> bool {
    if matches!(node.kind, NodeKind::Element(Primitive::Scroll)) {
        return true;
    }
    matches!(
        node.properties.get(&PropertyId::OVERFLOW),
        Some(Value::Text(value)) if matches!(value.as_str(), "scroll" | "auto")
    )
}

fn build_scroll_metrics(
    tree: &TreeMirror,
    boxes: &BTreeMap<NodeId, LayoutBox>,
    offsets: &BTreeMap<NodeId, (f64, f64)>,
) -> BTreeMap<NodeId, ScrollMetrics> {
    let mut result = BTreeMap::new();
    for node in tree.nodes() {
        if !scroll_container(&node) {
            continue;
        }
        let Some(layout) = boxes.get(&node.id) else {
            continue;
        };
        let (offset_x, offset_y) = offsets.get(&node.id).copied().unwrap_or((0.0, 0.0));
        let mut content_width = 0.0_f64;
        let mut content_height = 0.0_f64;
        for child in node.children {
            let Some(child) = boxes.get(&child) else {
                continue;
            };
            content_width =
                content_width.max(child.rect.x + child.rect.width + offset_x - layout.content.x);
            content_height =
                content_height.max(child.rect.y + child.rect.height + offset_y - layout.content.y);
        }
        result.insert(
            node.id,
            ScrollMetrics {
                node: node.id,
                viewport: layout.content,
                offset_x,
                offset_y,
                max_offset_x: (content_width - layout.content.width).max(0.0),
                max_offset_y: (content_height - layout.content.height).max(0.0),
            },
        );
    }
    result
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct GridAreaBounds {
    row: usize,
    column: usize,
    row_span: usize,
    column_span: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct GridAreaTemplate {
    rows: usize,
    columns: usize,
    areas: BTreeMap<String, GridAreaBounds>,
}

impl GridAreaTemplate {
    const fn column_count(&self) -> usize {
        self.columns
    }
}

fn grid_area_name(node: &NodeSnapshot, id: NodeId) -> Result<Option<String>, LayoutError> {
    let Some(value) = node.properties.get(&PropertyId::GRID_AREA) else {
        return Ok(None);
    };
    let Value::Text(name) = value else {
        return Err(LayoutError::InvalidGridAreas(id));
    };
    if !valid_grid_area_name(name) {
        return Err(LayoutError::InvalidGridAreas(id));
    }
    Ok(Some(name.clone()))
}

fn grid_template_areas(
    node: &NodeSnapshot,
    id: NodeId,
    limits: LayoutLimits,
) -> Result<Option<GridAreaTemplate>, LayoutError> {
    let Some(value) = node.properties.get(&PropertyId::GRID_TEMPLATE_AREAS) else {
        return Ok(None);
    };
    let Value::Text(value) = value else {
        return Err(LayoutError::InvalidGridAreas(id));
    };
    let mut cells = Vec::new();
    let mut rows = 0_usize;
    let mut columns = None;
    for row in value.split('/') {
        let names = row.split_ascii_whitespace().collect::<Vec<_>>();
        if names.is_empty() || names.len() > limits.max_grid_tracks {
            return Err(LayoutError::InvalidGridAreas(id));
        }
        if columns
            .replace(names.len())
            .is_some_and(|count| count != names.len())
        {
            return Err(LayoutError::InvalidGridAreas(id));
        }
        rows += 1;
        if rows > limits.max_grid_tracks
            || rows
                .checked_mul(names.len())
                .is_none_or(|count| count > limits.max_grid_area_cells)
        {
            return Err(LayoutError::GridAreaLimitExceeded(id));
        }
        for name in names {
            if name != "." && !valid_grid_area_name(name) {
                return Err(LayoutError::InvalidGridAreas(id));
            }
            cells.push(name.to_string());
        }
    }
    let Some(columns) = columns else {
        return Err(LayoutError::InvalidGridAreas(id));
    };
    let mut extents = BTreeMap::<String, (usize, usize, usize, usize)>::new();
    for (index, name) in cells.iter().enumerate() {
        if name == "." {
            continue;
        }
        let row = index / columns;
        let column = index % columns;
        extents
            .entry(name.clone())
            .and_modify(|bounds| {
                bounds.0 = bounds.0.min(row);
                bounds.1 = bounds.1.max(row);
                bounds.2 = bounds.2.min(column);
                bounds.3 = bounds.3.max(column);
            })
            .or_insert((row, row, column, column));
    }
    let mut areas = BTreeMap::new();
    for (name, (row_start, row_end, column_start, column_end)) in extents {
        for row in row_start..=row_end {
            for column in column_start..=column_end {
                if cells[row * columns + column] != name {
                    return Err(LayoutError::InvalidGridAreas(id));
                }
            }
        }
        areas.insert(
            name,
            GridAreaBounds {
                row: row_start,
                column: column_start,
                row_span: row_end - row_start + 1,
                column_span: column_end - column_start + 1,
            },
        );
    }
    Ok(Some(GridAreaTemplate {
        rows,
        columns,
        areas,
    }))
}

fn valid_grid_area_name(name: &str) -> bool {
    let mut bytes = name.bytes();
    let Some(first) = bytes.next() else {
        return false;
    };
    (first.is_ascii_alphabetic() || first == b'_')
        && bytes.all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
}

fn grid_tracks(
    node: &NodeSnapshot,
    id: NodeId,
    max_tracks: usize,
    default_count: Option<usize>,
) -> Result<Vec<GridTrack>, LayoutError> {
    let Some(value) = node.properties.get(&PropertyId::GRID_COLUMNS) else {
        return Ok(alloc::vec![
            GridTrack::Fraction(1.0);
            default_count.unwrap_or(1)
        ]);
    };
    let Value::Text(value) = value else {
        return Err(LayoutError::InvalidGridTracks(id));
    };
    let mut tracks = Vec::new();
    parse_grid_track_list(value, id, max_tracks, 0, &mut tracks)?;
    if tracks.is_empty() {
        return Err(LayoutError::InvalidGridTracks(id));
    }
    Ok(tracks)
}

fn parse_grid_track_list(
    value: &str,
    id: NodeId,
    max_tracks: usize,
    depth: usize,
    tracks: &mut Vec<GridTrack>,
) -> Result<(), LayoutError> {
    if depth >= MAX_GRID_SYNTAX_DEPTH {
        return Err(LayoutError::InvalidGridTracks(id));
    }
    for token in split_top_level(value, None, id)? {
        if let Some(body) = function_body(token, "repeat") {
            let arguments = split_top_level(body, Some(b','), id)?;
            let [count, repeated] = arguments.as_slice() else {
                return Err(LayoutError::InvalidGridTracks(id));
            };
            let count = count
                .trim()
                .parse::<usize>()
                .map_err(|_| LayoutError::InvalidGridTracks(id))?;
            if count == 0 {
                return Err(LayoutError::InvalidGridTracks(id));
            }
            let mut expansion = Vec::new();
            parse_grid_track_list(repeated.trim(), id, max_tracks, depth + 1, &mut expansion)?;
            if expansion.is_empty()
                || count > max_tracks.saturating_sub(tracks.len()) / expansion.len()
            {
                return Err(LayoutError::GridTrackLimitExceeded(id));
            }
            for _ in 0..count {
                tracks.extend(expansion.iter().copied());
            }
            continue;
        }
        if tracks.len() >= max_tracks {
            return Err(LayoutError::GridTrackLimitExceeded(id));
        }
        tracks.push(parse_grid_track(token, id)?);
    }
    Ok(())
}

fn parse_grid_track(token: &str, id: NodeId) -> Result<GridTrack, LayoutError> {
    if let Some(body) = function_body(token, "minmax") {
        let arguments = split_top_level(body, Some(b','), id)?;
        let [min, max] = arguments.as_slice() else {
            return Err(LayoutError::InvalidGridTracks(id));
        };
        let min = parse_grid_breadth(min.trim(), id)?;
        if matches!(min, GridBreadth::Fraction(_)) {
            return Err(LayoutError::InvalidGridTracks(id));
        }
        return Ok(GridTrack::MinMax {
            min,
            max: parse_grid_breadth(max.trim(), id)?,
        });
    }
    Ok(match parse_grid_breadth(token, id)? {
        GridBreadth::Px(value) => GridTrack::Px(value),
        GridBreadth::Fraction(value) => GridTrack::Fraction(value),
        GridBreadth::Auto => GridTrack::Auto,
    })
}

fn parse_grid_breadth(token: &str, id: NodeId) -> Result<GridBreadth, LayoutError> {
    if token == "auto" {
        return Ok(GridBreadth::Auto);
    }
    if let Some(number) = token.strip_suffix("fr") {
        let fraction = parse_grid_number(number, id)?;
        if fraction <= 0.0 {
            return Err(LayoutError::InvalidGridTracks(id));
        }
        return Ok(GridBreadth::Fraction(fraction));
    }
    let number = token.strip_suffix("px").unwrap_or(token);
    let pixels = parse_grid_number(number, id)?;
    if pixels < 0.0 {
        return Err(LayoutError::InvalidGridTracks(id));
    }
    Ok(GridBreadth::Px(pixels))
}

fn parse_grid_number(value: &str, id: NodeId) -> Result<f64, LayoutError> {
    let number = value
        .parse::<f64>()
        .map_err(|_| LayoutError::InvalidGridTracks(id))?;
    number
        .is_finite()
        .then_some(number)
        .ok_or(LayoutError::InvalidGridTracks(id))
}

fn function_body<'a>(token: &'a str, name: &str) -> Option<&'a str> {
    token
        .strip_prefix(name)?
        .strip_prefix('(')?
        .strip_suffix(')')
}

/// Splits either a top-level whitespace list or a comma-separated function
/// argument list while retaining nested function calls as one token.
fn split_top_level(
    value: &str,
    separator: Option<u8>,
    id: NodeId,
) -> Result<Vec<&str>, LayoutError> {
    let bytes = value.as_bytes();
    let mut parts = Vec::new();
    let mut start = None;
    let mut depth = 0_usize;
    for (index, byte) in bytes.iter().copied().enumerate() {
        match byte {
            b'(' => {
                depth = depth
                    .checked_add(1)
                    .ok_or(LayoutError::InvalidGridTracks(id))?;
                if depth > MAX_GRID_SYNTAX_DEPTH {
                    return Err(LayoutError::InvalidGridTracks(id));
                }
                start.get_or_insert(index);
            }
            b')' => {
                if depth == 0 {
                    return Err(LayoutError::InvalidGridTracks(id));
                }
                depth -= 1;
                start.get_or_insert(index);
            }
            _ => {
                let splits = depth == 0
                    && match separator {
                        Some(separator) => byte == separator,
                        None => byte.is_ascii_whitespace(),
                    };
                if splits {
                    if let Some(begin) = start.take() {
                        let part = value[begin..index].trim();
                        if !part.is_empty() {
                            parts.push(part);
                        }
                    } else if separator.is_some() {
                        return Err(LayoutError::InvalidGridTracks(id));
                    }
                } else if !byte.is_ascii_whitespace() || separator.is_some() || depth > 0 {
                    start.get_or_insert(index);
                }
            }
        }
    }
    if depth != 0 {
        return Err(LayoutError::InvalidGridTracks(id));
    }
    if let Some(begin) = start {
        let part = value[begin..].trim();
        if !part.is_empty() {
            parts.push(part);
        }
    } else if separator.is_some() && !value.trim().is_empty() {
        return Err(LayoutError::InvalidGridTracks(id));
    }
    Ok(parts)
}

struct GridMetrics {
    width: f64,
    height: f64,
    columns: Vec<f64>,
    rows: Vec<f64>,
    placements: Vec<Option<GridPlacement>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct GridPlacement {
    row: usize,
    column: usize,
    row_span: usize,
    column_span: usize,
}

fn grid_metrics(
    children: &[MeasuredNode],
    tracks: &[GridTrack],
    areas: Option<&GridAreaTemplate>,
    available_width: f64,
    gap: f64,
) -> GridMetrics {
    let column_count = tracks.len().max(1);
    let placements = grid_placements(children, areas, column_count);
    let mut intrinsic_columns = alloc::vec![0.0_f64; column_count];
    for (child, placement) in children.iter().zip(&placements) {
        let Some(placement) = placement else { continue };
        let contribution =
            (child.size.width - gap * placement.column_span.saturating_sub(1) as f64).max(0.0)
                / placement.column_span as f64;
        for column in
            &mut intrinsic_columns[placement.column..placement.column + placement.column_span]
        {
            *column = column.max(contribution);
        }
    }
    let mut columns = alloc::vec![0.0_f64; column_count];
    let mut fractions = alloc::vec![0.0_f64; column_count];
    let mut growth_limits = alloc::vec![None; column_count];
    for (index, track) in tracks.iter().copied().enumerate() {
        let intrinsic = intrinsic_columns[index];
        match track {
            GridTrack::Px(value) => columns[index] = value,
            GridTrack::Fraction(value) => fractions[index] = value,
            GridTrack::Auto => columns[index] = intrinsic,
            GridTrack::MinMax { min, max } => {
                columns[index] = match min {
                    GridBreadth::Px(value) => value,
                    GridBreadth::Auto => intrinsic,
                    GridBreadth::Fraction(_) => 0.0,
                };
                match max {
                    GridBreadth::Px(value) => {
                        growth_limits[index] = Some(value.max(columns[index]));
                    }
                    GridBreadth::Fraction(value) => fractions[index] = value,
                    GridBreadth::Auto => columns[index] = columns[index].max(intrinsic),
                }
            }
        }
    }
    let gaps = gap * column_count.saturating_sub(1) as f64;
    let content_width = (available_width - gaps).max(0.0);
    let mut free = (content_width - columns.iter().sum::<f64>()).max(0.0);
    grow_capped_tracks(&mut columns, &growth_limits, &mut free);
    resolve_fraction_tracks(&mut columns, &fractions, free);

    let row_count = placements
        .iter()
        .filter_map(|placement| placement.map(|placement| placement.row + placement.row_span))
        .max()
        .unwrap_or_else(|| areas.map_or(0, |areas| areas.rows));
    let mut rows = alloc::vec![0.0_f64; row_count];
    for (child, placement) in children.iter().zip(&placements) {
        let Some(placement) = placement else { continue };
        let contribution = (child.size.height - gap * placement.row_span.saturating_sub(1) as f64)
            .max(0.0)
            / placement.row_span as f64;
        for row in &mut rows[placement.row..placement.row + placement.row_span] {
            *row = row.max(contribution);
        }
    }
    GridMetrics {
        width: columns.iter().sum::<f64>() + gaps,
        height: rows.iter().sum::<f64>() + gap * rows.len().saturating_sub(1) as f64,
        columns,
        rows,
        placements,
    }
}

fn grid_placements(
    children: &[MeasuredNode],
    areas: Option<&GridAreaTemplate>,
    columns: usize,
) -> Vec<Option<GridPlacement>> {
    let mut placements = alloc::vec![None; children.len()];
    let initial_cells = areas.map_or(0, |areas| areas.rows * columns);
    let mut occupied = alloc::vec![false; initial_cells];
    for (index, child) in children.iter().enumerate() {
        if child.portal_layer.is_some() {
            continue;
        }
        let Some(bounds) = child
            .grid_area
            .as_ref()
            .and_then(|name| areas.and_then(|areas| areas.areas.get(name)))
        else {
            continue;
        };
        let placement = GridPlacement {
            row: bounds.row,
            column: bounds.column,
            row_span: bounds.row_span,
            column_span: bounds.column_span,
        };
        for row in placement.row..placement.row + placement.row_span {
            let end = row * columns + placement.column + placement.column_span;
            if occupied.len() < end {
                occupied.resize(end, false);
            }
            for column in placement.column..placement.column + placement.column_span {
                occupied[row * columns + column] = true;
            }
        }
        placements[index] = Some(placement);
    }
    let mut cursor = 0_usize;
    for (index, placement) in placements.iter_mut().enumerate() {
        if children[index].portal_layer.is_some() {
            continue;
        }
        if placement.is_some() {
            continue;
        }
        while occupied.get(cursor).copied().unwrap_or(false) {
            cursor += 1;
        }
        if occupied.len() <= cursor {
            occupied.resize(cursor + 1, false);
        }
        occupied[cursor] = true;
        *placement = Some(GridPlacement {
            row: cursor / columns,
            column: cursor % columns,
            row_span: 1,
            column_span: 1,
        });
        cursor += 1;
    }
    placements
}

fn grow_capped_tracks(columns: &mut [f64], limits: &[Option<f64>], free: &mut f64) {
    let mut active = limits
        .iter()
        .enumerate()
        .filter_map(|(index, limit)| limit.filter(|limit| *limit > columns[index]).map(|_| index))
        .collect::<Vec<_>>();
    while *free > 0.0 && !active.is_empty() {
        let share = *free / active.len() as f64;
        let mut consumed = 0.0;
        let mut next = Vec::new();
        for index in active {
            let capacity = limits[index].unwrap_or(columns[index]) - columns[index];
            let growth = capacity.min(share);
            columns[index] += growth;
            consumed += growth;
            if growth < capacity {
                next.push(index);
            }
        }
        if consumed <= f64::EPSILON {
            break;
        }
        *free = (*free - consumed).max(0.0);
        active = next;
    }
}

fn resolve_fraction_tracks(columns: &mut [f64], fractions: &[f64], free: f64) {
    let mut active = fractions
        .iter()
        .enumerate()
        .filter_map(|(index, factor)| (*factor > 0.0).then_some(index))
        .collect::<Vec<_>>();
    if active.is_empty() {
        return;
    }
    let mut available = active.iter().map(|index| columns[*index]).sum::<f64>() + free;
    loop {
        let total = active.iter().map(|index| fractions[*index]).sum::<f64>();
        let mut constrained = Vec::new();
        for index in &active {
            let share = available * fractions[*index] / total;
            if share < columns[*index] {
                constrained.push(*index);
            }
        }
        if constrained.is_empty() {
            for index in active {
                columns[index] = available * fractions[index] / total;
            }
            return;
        }
        active.retain(|index| !constrained.contains(index));
        for index in constrained {
            available = (available - columns[index]).max(0.0);
        }
        if active.is_empty() {
            return;
        }
    }
}

fn default_padding(node: &NodeSnapshot) -> f64 {
    match node.kind {
        NodeKind::Element(Primitive::Button) => 8.0,
        _ => 0.0,
    }
}

fn sum_main(children: &[MeasuredNode], gap: f64, horizontal: bool) -> f64 {
    let flowing = children
        .iter()
        .filter(|child| child.portal_layer.is_none())
        .collect::<Vec<_>>();
    let children_size = flowing
        .iter()
        .map(|child| {
            if horizontal {
                child.size.width
            } else {
                child.size.height
            }
        })
        .sum::<f64>();
    children_size + gap * flowing.len().saturating_sub(1) as f64
}

fn max_cross(children: &[MeasuredNode], horizontal: bool) -> f64 {
    children
        .iter()
        .filter(|child| child.portal_layer.is_none())
        .fold(0.0_f64, |value, child| {
            value.max(if horizontal {
                child.size.height
            } else {
                child.size.width
            })
        })
}

fn constrain_size(
    node: &NodeSnapshot,
    id: NodeId,
    available: Size,
    viewport: Size,
    size: &mut Size,
) -> Result<(), LayoutError> {
    if let Some(value) =
        length_property(node, PropertyId::MIN_WIDTH, available.width, viewport, id)?
    {
        size.width = size.width.max(value);
    }
    if let Some(value) =
        length_property(node, PropertyId::MIN_HEIGHT, available.height, viewport, id)?
    {
        size.height = size.height.max(value);
    }
    if let Some(value) =
        length_property(node, PropertyId::MAX_WIDTH, available.width, viewport, id)?
    {
        size.width = size.width.min(value);
    }
    if let Some(value) =
        length_property(node, PropertyId::MAX_HEIGHT, available.height, viewport, id)?
    {
        size.height = size.height.min(value);
    }
    Ok(())
}

fn number_property(
    node: &NodeSnapshot,
    property: PropertyId,
    id: NodeId,
) -> Result<Option<f64>, LayoutError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    let number = match value {
        Value::I64(value) => *value as f64,
        Value::F64(value) => *value,
        Value::Length(Length::Px(value)) => f64::from(*value),
        _ => return Ok(None),
    };
    if !number.is_finite() {
        return Err(LayoutError::InvalidNumber(id, property));
    }
    Ok(Some(number))
}

fn length_property(
    node: &NodeSnapshot,
    property: PropertyId,
    containing: f64,
    viewport: Size,
    id: NodeId,
) -> Result<Option<f64>, LayoutError> {
    let Some(value) = node.properties.get(&property) else {
        return Ok(None);
    };
    let value = match value {
        Value::Length(Length::Auto) => return Ok(None),
        Value::Length(Length::Px(value)) => f64::from(*value),
        Value::Length(Length::Percent(value)) => containing * f64::from(*value) / 100.0,
        Value::Length(Length::ViewportWidth(value)) => viewport.width * f64::from(*value) / 100.0,
        Value::Length(Length::ViewportHeight(value)) => viewport.height * f64::from(*value) / 100.0,
        Value::I64(value) => *value as f64,
        Value::F64(value) => *value,
        _ => return Ok(None),
    };
    if !value.is_finite() || value < 0.0 {
        return Err(LayoutError::InvalidNumber(id, property));
    }
    Ok(Some(value))
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_ui_core::{Property, Value};
    use vo_ui_protocol::{Mutation, MutationBatch};

    fn tree(mutations: Vec<Mutation>) -> TreeMirror {
        let root = NodeId::new(0, 1);
        let mut tree = TreeMirror::new(7, root, vo_ui_protocol::ProtocolLimits::default());
        tree.apply(&MutationBatch::new(7, 1, mutations)).unwrap();
        tree
    }

    #[test]
    fn column_gap_padding_and_text_measure_deterministically() {
        let root = NodeId::new(0, 1);
        let column = NodeId::new(1, 1);
        let first = NodeId::new(2, 1);
        let second = NodeId::new(3, 1);
        let tree = tree(alloc::vec![
            Mutation::Create {
                id: column,
                kind: NodeKind::Element(Primitive::Column)
            },
            Mutation::SetProperty {
                id: column,
                property: Property::new(PropertyId::PADDING, Value::Length(Length::Px(10.0)))
            },
            Mutation::SetProperty {
                id: column,
                property: Property::new(PropertyId::GAP, Value::Length(Length::Px(4.0)))
            },
            Mutation::Create {
                id: first,
                kind: NodeKind::Text
            },
            Mutation::SetText {
                id: first,
                text: "abcd".into()
            },
            Mutation::Create {
                id: second,
                kind: NodeKind::Text
            },
            Mutation::SetText {
                id: second,
                text: "xy".into()
            },
            Mutation::InsertBefore {
                parent: column,
                child: first,
                before: None
            },
            Mutation::InsertBefore {
                parent: column,
                child: second,
                before: None
            },
            Mutation::InsertBefore {
                parent: root,
                child: column,
                before: None
            },
        ]);
        let snapshot = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(snapshot.len(), 4);
        let column_box = snapshot.get(column).unwrap();
        assert_eq!(column_box.rect.width, 300.0);
        assert_eq!(column_box.content.x, 10.0);
        assert_eq!(snapshot.get(first).unwrap().rect.y, 10.0);
        assert_eq!(snapshot.get(second).unwrap().rect.y, 31.5);
    }

    #[test]
    fn semantic_text_element_inherits_font_size_and_wraps_character_node() {
        let root = NodeId::new(0, 1);
        let element = NodeId::new(1, 1);
        let text = NodeId::new(2, 1);
        let tree = tree(alloc::vec![
            Mutation::Create {
                id: element,
                kind: NodeKind::Element(Primitive::Text),
            },
            Mutation::SetProperty {
                id: element,
                property: Property::new(PropertyId::FONT_SIZE, 20_i64),
            },
            Mutation::Create {
                id: text,
                kind: NodeKind::Text,
            },
            Mutation::SetText {
                id: text,
                text: "abcd".into(),
            },
            Mutation::InsertBefore {
                parent: element,
                child: text,
                before: None,
            },
            Mutation::InsertBefore {
                parent: root,
                child: element,
                before: None,
            },
        ]);
        let snapshot = compute_layout(
            &tree,
            Size::new(200.0, 100.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(
            snapshot.get(element).unwrap().rect,
            Rect::new(0.0, 0.0, 44.0, 25.0)
        );
        assert_eq!(
            snapshot.get(text).unwrap().rect,
            Rect::new(0.0, 0.0, 44.0, 25.0)
        );
    }

    #[test]
    fn hidden_subtree_is_excluded_from_layout_and_sibling_flow() {
        let root = NodeId::new(0, 1);
        let column = NodeId::new(1, 1);
        let hidden = NodeId::new(2, 1);
        let hidden_child = NodeId::new(3, 1);
        let visible = NodeId::new(4, 1);
        let tree = tree(alloc::vec![
            Mutation::Create {
                id: column,
                kind: NodeKind::Element(Primitive::Column),
            },
            Mutation::Create {
                id: hidden,
                kind: NodeKind::Element(Primitive::Box),
            },
            Mutation::SetProperty {
                id: hidden,
                property: Property::new(PropertyId::HIDDEN, true),
            },
            Mutation::SetProperty {
                id: hidden,
                property: Property::new(PropertyId::HEIGHT, 80_i64),
            },
            Mutation::Create {
                id: hidden_child,
                kind: NodeKind::Element(Primitive::Box),
            },
            Mutation::Create {
                id: visible,
                kind: NodeKind::Element(Primitive::Box),
            },
            Mutation::SetProperty {
                id: visible,
                property: Property::new(PropertyId::HEIGHT, 20_i64),
            },
            Mutation::InsertBefore {
                parent: hidden,
                child: hidden_child,
                before: None,
            },
            Mutation::InsertBefore {
                parent: column,
                child: hidden,
                before: None,
            },
            Mutation::InsertBefore {
                parent: column,
                child: visible,
                before: None,
            },
            Mutation::InsertBefore {
                parent: root,
                child: column,
                before: None,
            },
        ]);
        let snapshot = compute_layout(
            &tree,
            Size::new(200.0, 120.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert!(snapshot.get(hidden).is_none());
        assert!(snapshot.get(hidden_child).is_none());
        assert_eq!(snapshot.get(visible).unwrap().rect.y, 0.0);
    }

    #[test]
    fn row_flex_distributes_remaining_space() {
        let root = NodeId::new(0, 1);
        let row = NodeId::new(1, 1);
        let fixed = NodeId::new(2, 1);
        let flexible = NodeId::new(3, 1);
        let tree = tree(alloc::vec![
            Mutation::Create {
                id: row,
                kind: NodeKind::Element(Primitive::Row)
            },
            Mutation::SetProperty {
                id: row,
                property: Property::new(PropertyId::HEIGHT, 40_i64)
            },
            Mutation::Create {
                id: fixed,
                kind: NodeKind::Element(Primitive::Box)
            },
            Mutation::SetProperty {
                id: fixed,
                property: Property::new(PropertyId::WIDTH, 50_i64)
            },
            Mutation::Create {
                id: flexible,
                kind: NodeKind::Element(Primitive::Box)
            },
            Mutation::SetProperty {
                id: flexible,
                property: Property::new(PropertyId::WIDTH, 10_i64)
            },
            Mutation::SetProperty {
                id: flexible,
                property: Property::new(PropertyId::FLEX, 1_i64)
            },
            Mutation::InsertBefore {
                parent: row,
                child: fixed,
                before: None
            },
            Mutation::InsertBefore {
                parent: row,
                child: flexible,
                before: None
            },
            Mutation::InsertBefore {
                parent: root,
                child: row,
                before: None
            },
        ]);
        let snapshot = compute_layout(
            &tree,
            Size::new(200.0, 100.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(snapshot.get(fixed).unwrap().rect.width, 50.0);
        assert_eq!(snapshot.get(flexible).unwrap().rect.width, 150.0);
        assert_eq!(snapshot.get(flexible).unwrap().rect.x, 50.0);
    }

    #[test]
    fn inherited_rtl_places_row_children_from_the_logical_start_edge() {
        let root = NodeId::new(0, 1);
        let row = NodeId::new(1, 1);
        let first = NodeId::new(2, 1);
        let second = NodeId::new(3, 1);
        let tree = tree(alloc::vec![
            Mutation::SetProperty {
                id: root,
                property: Property::new(PropertyId::FLOW_DIRECTION, 1_i64),
            },
            Mutation::Create {
                id: row,
                kind: NodeKind::Element(Primitive::Row),
            },
            Mutation::Create {
                id: first,
                kind: NodeKind::Element(Primitive::Box),
            },
            Mutation::SetProperty {
                id: first,
                property: Property::new(PropertyId::WIDTH, 40_i64),
            },
            Mutation::Create {
                id: second,
                kind: NodeKind::Element(Primitive::Box),
            },
            Mutation::SetProperty {
                id: second,
                property: Property::new(PropertyId::WIDTH, 30_i64),
            },
            Mutation::InsertBefore {
                parent: row,
                child: first,
                before: None,
            },
            Mutation::InsertBefore {
                parent: row,
                child: second,
                before: None,
            },
            Mutation::InsertBefore {
                parent: root,
                child: row,
                before: None,
            },
        ]);
        let snapshot = compute_layout(
            &tree,
            Size::new(200.0, 100.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(snapshot.get(first).unwrap().rect.x, 160.0);
        assert_eq!(snapshot.get(second).unwrap().rect.x, 130.0);
    }

    #[test]
    fn portal_subtree_uses_viewport_bounds_and_does_not_consume_parent_flow() {
        let root = NodeId::new(0, 1);
        let column = NodeId::new(1, 1);
        let content = NodeId::new(2, 1);
        let portal = NodeId::new(3, 1);
        let portal_button = NodeId::new(4, 1);
        let tree = tree(alloc::vec![
            Mutation::Create {
                id: column,
                kind: NodeKind::Element(Primitive::Column),
            },
            Mutation::Create {
                id: content,
                kind: NodeKind::Element(Primitive::Box),
            },
            Mutation::SetProperty {
                id: content,
                property: Property::new(PropertyId::HEIGHT, 40_i64),
            },
            Mutation::Create {
                id: portal,
                kind: NodeKind::Element(Primitive::Stack),
            },
            Mutation::SetProperty {
                id: portal,
                property: Property::new(PropertyId::PORTAL_LAYER, 200_i64),
            },
            Mutation::Create {
                id: portal_button,
                kind: NodeKind::Element(Primitive::Button),
            },
            Mutation::InsertBefore {
                parent: portal,
                child: portal_button,
                before: None,
            },
            Mutation::InsertBefore {
                parent: column,
                child: content,
                before: None,
            },
            Mutation::InsertBefore {
                parent: column,
                child: portal,
                before: None,
            },
            Mutation::InsertBefore {
                parent: root,
                child: column,
                before: None,
            },
        ]);
        let snapshot = compute_layout(
            &tree,
            Size::new(320.0, 240.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(snapshot.get(content).unwrap().rect.y, 0.0);
        assert_eq!(
            snapshot.get(portal).unwrap().rect,
            Rect::new(0.0, 0.0, 320.0, 240.0)
        );
        assert!(snapshot.get(portal).unwrap().z_index >= 200_000);
        assert!(
            snapshot.get(portal_button).unwrap().z_index > snapshot.get(portal).unwrap().z_index
        );
    }

    #[test]
    fn invalid_dynamic_dimension_fails_closed() {
        let root = NodeId::new(0, 1);
        let child = NodeId::new(1, 1);
        let tree = tree(alloc::vec![
            Mutation::Create {
                id: child,
                kind: NodeKind::Element(Primitive::Box)
            },
            Mutation::SetProperty {
                id: child,
                property: Property::new(PropertyId::WIDTH, f64::NAN)
            },
            Mutation::InsertBefore {
                parent: root,
                child,
                before: None
            },
        ]);
        assert_eq!(
            compute_layout(
                &tree,
                Size::new(100.0, 100.0),
                LayoutLimits::default(),
                &mut ApproximateTextMeasurer,
            ),
            Err(LayoutError::InvalidNumber(child, PropertyId::WIDTH))
        );
    }

    #[test]
    fn grid_tracks_place_fixed_and_fraction_columns() {
        let root = NodeId::new(0, 1);
        let grid = NodeId::new(1, 1);
        let first = NodeId::new(2, 1);
        let second = NodeId::new(3, 1);
        let third = NodeId::new(4, 1);
        let fourth = NodeId::new(5, 1);
        let mut mutations = alloc::vec![
            Mutation::Create {
                id: grid,
                kind: NodeKind::Element(Primitive::Grid)
            },
            Mutation::SetProperty {
                id: grid,
                property: Property::new(PropertyId::GRID_COLUMNS, "100px 1fr 2fr")
            },
            Mutation::SetProperty {
                id: grid,
                property: Property::new(PropertyId::GAP, 10_i64)
            },
        ];
        for child in [first, second, third, fourth] {
            mutations.extend([
                Mutation::Create {
                    id: child,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::SetProperty {
                    id: child,
                    property: Property::new(PropertyId::HEIGHT, 20_i64),
                },
                Mutation::InsertBefore {
                    parent: grid,
                    child,
                    before: None,
                },
            ]);
        }
        mutations.push(Mutation::InsertBefore {
            parent: root,
            child: grid,
            before: None,
        });
        let snapshot = compute_layout(
            &tree(mutations),
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(snapshot.get(first).unwrap().rect.width, 100.0);
        assert_eq!(snapshot.get(second).unwrap().rect.width, 60.0);
        assert_eq!(snapshot.get(third).unwrap().rect.width, 120.0);
        assert_eq!(snapshot.get(second).unwrap().rect.x, 110.0);
        assert_eq!(snapshot.get(third).unwrap().rect.x, 180.0);
        assert_eq!(snapshot.get(fourth).unwrap().rect.y, 30.0);
    }

    #[test]
    fn grid_repeat_and_minmax_share_space_with_bounded_minimums() {
        let root = NodeId::new(0, 1);
        let grid = NodeId::new(1, 1);
        let children = [NodeId::new(2, 1), NodeId::new(3, 1), NodeId::new(4, 1)];
        let mut mutations = alloc::vec![
            Mutation::Create {
                id: grid,
                kind: NodeKind::Element(Primitive::Grid),
            },
            Mutation::SetProperty {
                id: grid,
                property: Property::new(PropertyId::GRID_COLUMNS, "repeat(3, minmax(80px, 1fr))",),
            },
            Mutation::SetProperty {
                id: grid,
                property: Property::new(PropertyId::GAP, 10_i64),
            },
        ];
        for child in children {
            mutations.extend([
                Mutation::Create {
                    id: child,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::SetProperty {
                    id: child,
                    property: Property::new(PropertyId::HEIGHT, 20_i64),
                },
                Mutation::InsertBefore {
                    parent: grid,
                    child,
                    before: None,
                },
            ]);
        }
        mutations.push(Mutation::InsertBefore {
            parent: root,
            child: grid,
            before: None,
        });
        let snapshot = compute_layout(
            &tree(mutations),
            Size::new(350.0, 100.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        for child in children {
            assert_eq!(snapshot.get(child).unwrap().rect.width, 110.0);
        }
        assert_eq!(snapshot.get(children[1]).unwrap().rect.x, 120.0);
        assert_eq!(snapshot.get(children[2]).unwrap().rect.x, 240.0);
    }

    #[test]
    fn grid_minmax_caps_fixed_growth_before_fraction_tracks() {
        let root = NodeId::new(0, 1);
        let grid = NodeId::new(1, 1);
        let first = NodeId::new(2, 1);
        let second = NodeId::new(3, 1);
        let snapshot = compute_layout(
            &tree(alloc::vec![
                Mutation::Create {
                    id: grid,
                    kind: NodeKind::Element(Primitive::Grid),
                },
                Mutation::SetProperty {
                    id: grid,
                    property: Property::new(PropertyId::GRID_COLUMNS, "minmax(40px, 80px) 1fr",),
                },
                Mutation::Create {
                    id: first,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::Create {
                    id: second,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::InsertBefore {
                    parent: grid,
                    child: first,
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: grid,
                    child: second,
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: grid,
                    before: None,
                },
            ]),
            Size::new(300.0, 100.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(snapshot.get(first).unwrap().rect.width, 80.0);
        assert_eq!(snapshot.get(second).unwrap().rect.width, 220.0);
    }

    #[test]
    fn named_grid_areas_place_and_span_children() {
        let root = NodeId::new(0, 1);
        let grid = NodeId::new(1, 1);
        let header = NodeId::new(2, 1);
        let sidebar = NodeId::new(3, 1);
        let main = NodeId::new(4, 1);
        let mut mutations = alloc::vec![
            Mutation::Create {
                id: grid,
                kind: NodeKind::Element(Primitive::Grid),
            },
            Mutation::SetProperty {
                id: grid,
                property: Property::new(PropertyId::GRID_COLUMNS, "100px 1fr"),
            },
            Mutation::SetProperty {
                id: grid,
                property: Property::new(
                    PropertyId::GRID_TEMPLATE_AREAS,
                    "header header / sidebar main",
                ),
            },
            Mutation::SetProperty {
                id: grid,
                property: Property::new(PropertyId::GAP, 10_i64),
            },
        ];
        for (child, area, height) in [
            (header, "header", 20_i64),
            (sidebar, "sidebar", 40_i64),
            (main, "main", 60_i64),
        ] {
            mutations.extend([
                Mutation::Create {
                    id: child,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::SetProperty {
                    id: child,
                    property: Property::new(PropertyId::GRID_AREA, area),
                },
                Mutation::SetProperty {
                    id: child,
                    property: Property::new(PropertyId::HEIGHT, height),
                },
                Mutation::InsertBefore {
                    parent: grid,
                    child,
                    before: None,
                },
            ]);
        }
        mutations.push(Mutation::InsertBefore {
            parent: root,
            child: grid,
            before: None,
        });
        let snapshot = compute_layout(
            &tree(mutations),
            Size::new(300.0, 120.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        assert_eq!(
            snapshot.get(header).unwrap().rect,
            Rect::new(0.0, 0.0, 300.0, 20.0)
        );
        assert_eq!(
            snapshot.get(sidebar).unwrap().rect,
            Rect::new(0.0, 30.0, 100.0, 60.0)
        );
        assert_eq!(
            snapshot.get(main).unwrap().rect,
            Rect::new(110.0, 30.0, 190.0, 60.0)
        );
    }

    #[test]
    fn named_grid_areas_reject_non_rectangles_and_unknown_assignments() {
        let root = NodeId::new(0, 1);
        let grid = NodeId::new(1, 1);
        let child = NodeId::new(2, 1);
        let area_tree = |template: &str, area: &str| {
            tree(alloc::vec![
                Mutation::Create {
                    id: grid,
                    kind: NodeKind::Element(Primitive::Grid),
                },
                Mutation::SetProperty {
                    id: grid,
                    property: Property::new(PropertyId::GRID_TEMPLATE_AREAS, template),
                },
                Mutation::Create {
                    id: child,
                    kind: NodeKind::Element(Primitive::Box),
                },
                Mutation::SetProperty {
                    id: child,
                    property: Property::new(PropertyId::GRID_AREA, area),
                },
                Mutation::InsertBefore {
                    parent: grid,
                    child,
                    before: None,
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: grid,
                    before: None,
                },
            ])
        };
        for (template, area) in [("a a / a b", "a"), ("header main", "missing")] {
            assert_eq!(
                compute_layout(
                    &area_tree(template, area),
                    Size::new(300.0, 100.0),
                    LayoutLimits::default(),
                    &mut ApproximateTextMeasurer,
                ),
                Err(LayoutError::InvalidGridAreas(grid)),
            );
        }
    }

    #[test]
    fn grid_advanced_syntax_fails_closed_at_depth_and_track_limits() {
        let root = NodeId::new(0, 1);
        let grid = NodeId::new(1, 1);
        let mutations = |columns: &str| {
            alloc::vec![
                Mutation::Create {
                    id: grid,
                    kind: NodeKind::Element(Primitive::Grid),
                },
                Mutation::SetProperty {
                    id: grid,
                    property: Property::new(PropertyId::GRID_COLUMNS, columns),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: grid,
                    before: None,
                },
            ]
        };
        let limits = LayoutLimits {
            max_grid_tracks: 3,
            ..LayoutLimits::default()
        };
        assert_eq!(
            compute_layout(
                &tree(mutations("repeat(4, 1fr)")),
                Size::new(300.0, 100.0),
                limits,
                &mut ApproximateTextMeasurer,
            ),
            Err(LayoutError::GridTrackLimitExceeded(grid))
        );
        for invalid in [
            "repeat(0, 1fr)",
            "minmax(1fr, 2fr)",
            "repeat(2, minmax(10px, 1fr)",
            "repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, repeat(1, 1fr)))))))))))))))))",
        ] {
            assert_eq!(
                compute_layout(
                    &tree(mutations(invalid)),
                    Size::new(300.0, 100.0),
                    LayoutLimits::default(),
                    &mut ApproximateTextMeasurer,
                ),
                Err(LayoutError::InvalidGridTracks(grid)),
                "syntax should be rejected: {invalid}",
            );
        }
    }

    #[test]
    fn scroll_offsets_and_clips_descendants() {
        let root = NodeId::new(0, 1);
        let scroll = NodeId::new(1, 1);
        let child = NodeId::new(2, 1);
        let tree = tree(alloc::vec![
            Mutation::Create {
                id: scroll,
                kind: NodeKind::Element(Primitive::Scroll)
            },
            Mutation::SetProperty {
                id: scroll,
                property: Property::new(PropertyId::WIDTH, 100_i64)
            },
            Mutation::SetProperty {
                id: scroll,
                property: Property::new(PropertyId::HEIGHT, 50_i64)
            },
            Mutation::SetProperty {
                id: scroll,
                property: Property::new(PropertyId::SCROLL_Y, 30_i64)
            },
            Mutation::Create {
                id: child,
                kind: NodeKind::Element(Primitive::Box)
            },
            Mutation::SetProperty {
                id: child,
                property: Property::new(PropertyId::HEIGHT, 100_i64)
            },
            Mutation::InsertBefore {
                parent: scroll,
                child,
                before: None
            },
            Mutation::InsertBefore {
                parent: root,
                child: scroll,
                before: None
            },
        ]);
        let snapshot = compute_layout(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
        )
        .unwrap();
        let child = snapshot.get(child).unwrap();
        assert_eq!(child.rect.y, -30.0);
        let clip = child.clip.unwrap();
        assert!(clip.contains(10.0, 10.0));
        assert!(!clip.contains(10.0, 60.0));
        assert_eq!(
            snapshot.scroll(scroll),
            Some(&ScrollMetrics {
                node: scroll,
                viewport: Rect::new(0.0, 0.0, 100.0, 50.0),
                offset_x: 0.0,
                offset_y: 30.0,
                max_offset_x: 0.0,
                max_offset_y: 50.0,
            })
        );

        struct Override;

        impl ScrollOffsetProvider for Override {
            fn resolve_scroll_offset(
                &mut self,
                node: NodeId,
                declared_x: f64,
                declared_y: f64,
            ) -> (f64, f64) {
                if node == NodeId::new(1, 1) {
                    (5.0, 45.0)
                } else {
                    (declared_x, declared_y)
                }
            }
        }

        let overridden = compute_layout_with_scroll_offsets(
            &tree,
            Size::new(300.0, 200.0),
            LayoutLimits::default(),
            &mut ApproximateTextMeasurer,
            &mut Override,
        )
        .unwrap();
        assert_eq!(overridden.get(NodeId::new(2, 1)).unwrap().rect.y, -45.0);
        assert_eq!(overridden.scroll(scroll).unwrap().offset_y, 45.0);
    }
}
