use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt::Write;

use vo_ui_core::{EventType, Length, NodeId, Primitive, PropertyId, Value};
use vo_ui_protocol::{NodeKind, NodeSnapshot, TreeMirror};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SsrLimits {
    pub max_nodes: usize,
    pub max_depth: usize,
    pub max_html_bytes: usize,
    pub max_metadata_bytes: usize,
    pub max_assets: usize,
    pub max_activation_entries: usize,
}

impl Default for SsrLimits {
    fn default() -> Self {
        Self {
            max_nodes: 100_000,
            max_depth: 1_024,
            max_html_bytes: 16 * 1024 * 1024,
            max_metadata_bytes: 256 * 1024,
            max_assets: 4_096,
            max_activation_entries: 100_000,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AssetLink {
    pub href: String,
    pub kind: String,
    pub integrity: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DocumentMetadata {
    pub language: String,
    pub direction: String,
    pub title: String,
    pub description: String,
    pub canonical_url: Option<String>,
    pub theme_color: Option<String>,
    pub nonce: Option<String>,
    pub assets: Vec<AssetLink>,
}

impl Default for DocumentMetadata {
    fn default() -> Self {
        Self {
            language: "en".to_string(),
            direction: "ltr".to_string(),
            title: String::new(),
            description: String::new(),
            canonical_url: None,
            theme_color: None,
            nonce: None,
            assets: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ActivationEntry {
    pub node: NodeId,
    pub events: Vec<EventType>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RenderedDocument {
    pub html: String,
    pub activation: Vec<ActivationEntry>,
    pub revision: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SsrError {
    InvalidLimits,
    InvalidMetadata,
    MissingNode(NodeId),
    NodeLimitExceeded,
    DepthLimitExceeded,
    HtmlLimitExceeded,
    MetadataLimitExceeded,
    ActivationLimitExceeded,
    InvalidProperty(NodeId, PropertyId),
}

fn valid_token(value: &str) -> bool {
    !value.is_empty()
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
}

fn validate(metadata: &DocumentMetadata, limits: SsrLimits) -> Result<(), SsrError> {
    if limits.max_nodes == 0
        || limits.max_depth == 0
        || limits.max_html_bytes == 0
        || limits.max_metadata_bytes == 0
        || limits.max_assets == 0
        || limits.max_activation_entries == 0
    {
        return Err(SsrError::InvalidLimits);
    }
    if !valid_token(&metadata.language)
        || !matches!(metadata.direction.as_str(), "ltr" | "rtl" | "auto")
        || metadata.assets.len() > limits.max_assets
        || metadata
            .nonce
            .as_deref()
            .is_some_and(|nonce| !valid_token(nonce))
    {
        return Err(SsrError::InvalidMetadata);
    }
    let metadata_bytes = metadata.language.len()
        + metadata.direction.len()
        + metadata.title.len()
        + metadata.description.len()
        + metadata.canonical_url.as_ref().map_or(0, String::len)
        + metadata.theme_color.as_ref().map_or(0, String::len)
        + metadata.nonce.as_ref().map_or(0, String::len)
        + metadata
            .assets
            .iter()
            .map(|asset| {
                asset.href.len()
                    + asset.kind.len()
                    + asset.integrity.as_ref().map_or(0, String::len)
            })
            .sum::<usize>();
    if metadata_bytes > limits.max_metadata_bytes
        || metadata.assets.iter().any(|asset| {
            asset.href.is_empty()
                || !matches!(
                    asset.kind.as_str(),
                    "style" | "preload" | "icon" | "manifest"
                )
                || asset
                    .integrity
                    .as_deref()
                    .is_some_and(|integrity| !integrity.starts_with("sha256-"))
        })
    {
        return Err(SsrError::MetadataLimitExceeded);
    }
    Ok(())
}

fn escape_text(output: &mut String, value: &str) {
    for character in value.chars() {
        match character {
            '&' => output.push_str("&amp;"),
            '<' => output.push_str("&lt;"),
            '>' => output.push_str("&gt;"),
            _ => output.push(character),
        }
    }
}

fn escape_attribute(output: &mut String, value: &str) {
    for character in value.chars() {
        match character {
            '&' => output.push_str("&amp;"),
            '<' => output.push_str("&lt;"),
            '>' => output.push_str("&gt;"),
            '"' => output.push_str("&quot;"),
            '\'' => output.push_str("&#39;"),
            _ => output.push(character),
        }
    }
}

fn text_property(node: &NodeSnapshot, id: PropertyId) -> Result<Option<&str>, SsrError> {
    match node.properties.get(&id) {
        None => Ok(None),
        Some(Value::Text(value)) => Ok(Some(value)),
        Some(_) => Err(SsrError::InvalidProperty(node.id, id)),
    }
}

fn bool_property(node: &NodeSnapshot, id: PropertyId) -> Result<Option<bool>, SsrError> {
    match node.properties.get(&id) {
        None => Ok(None),
        Some(Value::Bool(value)) => Ok(Some(*value)),
        Some(_) => Err(SsrError::InvalidProperty(node.id, id)),
    }
}

fn scalar_property(node: &NodeSnapshot, id: PropertyId) -> Result<Option<String>, SsrError> {
    match node.properties.get(&id) {
        None => Ok(None),
        Some(Value::Text(value)) => Ok(Some(value.clone())),
        Some(Value::I64(value)) => Ok(Some(value.to_string())),
        Some(Value::F64(value)) if value.is_finite() => Ok(Some(value.to_string())),
        Some(_) => Err(SsrError::InvalidProperty(node.id, id)),
    }
}

fn attribute(output: &mut String, name: &str, value: &str) {
    output.push(' ');
    output.push_str(name);
    output.push_str("=\"");
    escape_attribute(output, value);
    output.push('"');
}

fn boolean_attribute(output: &mut String, name: &str, value: Option<bool>) {
    if value == Some(true) {
        output.push(' ');
        output.push_str(name);
    }
}

fn aria_boolean(output: &mut String, name: &str, value: Option<bool>) {
    if let Some(value) = value {
        attribute(output, name, if value { "true" } else { "false" });
    }
}

fn length(value: &Length) -> String {
    match value {
        Length::Auto => "auto".to_string(),
        Length::Px(value) => format!("{value}px"),
        Length::Percent(value) => format!("{value}%"),
        Length::ViewportWidth(value) => format!("{value}vw"),
        Length::ViewportHeight(value) => format!("{value}vh"),
    }
}

fn style(node: &NodeSnapshot) -> Result<String, SsrError> {
    // Keep server HTML geometrically equivalent to the live DOM renderer.
    // Nested flex/grid children need zero minimums so a Scroll node can own
    // overflow instead of growing the browser document.
    let mut output = String::from("min-width:0;min-height:0;");
    if let NodeKind::Element(primitive) = node.kind {
        match primitive {
            Primitive::Fragment => output.push_str("display:contents;"),
            Primitive::Row => output.push_str("display:flex;flex-direction:row;"),
            Primitive::Column => output.push_str("display:flex;flex-direction:column;"),
            Primitive::Stack => output.push_str("position:relative;display:grid;"),
            Primitive::Grid => output.push_str("display:grid;"),
            Primitive::Scroll => output.push_str("overflow:auto;"),
            _ => {}
        }
    }
    for (id, value) in &node.properties {
        let name = match *id {
            PropertyId::WIDTH => "width",
            PropertyId::HEIGHT => "height",
            PropertyId::MIN_WIDTH => "min-width",
            PropertyId::MIN_HEIGHT => "min-height",
            PropertyId::MAX_WIDTH => "max-width",
            PropertyId::MAX_HEIGHT => "max-height",
            PropertyId::GAP => "gap",
            PropertyId::PADDING => "padding",
            PropertyId::FONT_SIZE => "font-size",
            PropertyId::RADIUS => "border-radius",
            PropertyId::BORDER_WIDTH => "border-width",
            _ => continue,
        };
        let rendered = match value {
            Value::Length(value) => length(value),
            Value::F64(value) if value.is_finite() => format!("{value}px"),
            Value::I64(value) => format!("{value}px"),
            _ => return Err(SsrError::InvalidProperty(node.id, *id)),
        };
        write!(output, "{name}:{rendered};").expect("writing to String cannot fail");
    }
    if let Some(Value::F64(value)) = node.properties.get(&PropertyId::FLEX) {
        write!(output, "flex:{value};").expect("writing to String cannot fail");
    }
    if let Some(Value::Color(value)) = node.properties.get(&PropertyId::BACKGROUND) {
        let [alpha, red, green, blue] = value.to_be_bytes();
        write!(
            output,
            "background-color:#{red:02x}{green:02x}{blue:02x}{alpha:02x};"
        )
        .expect("writing to String cannot fail");
    }
    if let Some(Value::Color(value)) = node.properties.get(&PropertyId::FOREGROUND) {
        let [alpha, red, green, blue] = value.to_be_bytes();
        write!(output, "color:#{red:02x}{green:02x}{blue:02x}{alpha:02x};")
            .expect("writing to String cannot fail");
    }
    if let Some(Value::Color(value)) = node.properties.get(&PropertyId::BORDER_COLOR) {
        let [alpha, red, green, blue] = value.to_be_bytes();
        write!(
            output,
            "border-color:#{red:02x}{green:02x}{blue:02x}{alpha:02x};border-style:solid;"
        )
        .expect("writing to String cannot fail");
    }
    if let Some(Value::I64(value)) = node.properties.get(&PropertyId::FONT_WEIGHT) {
        write!(output, "font-weight:{value};").expect("writing to String cannot fail");
    }
    if let Some(Value::Text(value)) = node.properties.get(&PropertyId::GRID_COLUMNS) {
        write!(output, "grid-template-columns:{value};").expect("writing to String cannot fail");
    }
    if let Some(value) = node.properties.get(&PropertyId::OPACITY) {
        let Value::F64(value) = value else {
            return Err(SsrError::InvalidProperty(node.id, PropertyId::OPACITY));
        };
        if !value.is_finite() || !(0.0..=1.0).contains(value) {
            return Err(SsrError::InvalidProperty(node.id, PropertyId::OPACITY));
        }
        write!(output, "opacity:{value};").expect("writing to String cannot fail");
    }
    if let Some(value) = text_property(node, PropertyId::FIT)? {
        if !matches!(value, "contain" | "cover" | "fill" | "none" | "scale-down") {
            return Err(SsrError::InvalidProperty(node.id, PropertyId::FIT));
        }
        write!(output, "object-fit:{value};").expect("writing to String cannot fail");
    }
    if let Some(value) = text_property(node, PropertyId::TRANSFORM)? {
        if value.len() > 512
            || !value.bytes().all(|byte| {
                byte.is_ascii_alphanumeric()
                    || matches!(byte, b'(' | b')' | b',' | b'.' | b'-' | b'+' | b' ')
            })
        {
            return Err(SsrError::InvalidProperty(node.id, PropertyId::TRANSFORM));
        }
        write!(output, "transform:{value};").expect("writing to String cannot fail");
    }
    Ok(output)
}

fn tag(primitive: Primitive) -> (&'static str, bool) {
    match primitive {
        Primitive::Root => ("div", false),
        Primitive::Fragment => ("div", false),
        Primitive::Box
        | Primitive::Row
        | Primitive::Column
        | Primitive::Stack
        | Primitive::Grid => ("div", false),
        Primitive::Scroll => ("div", false),
        Primitive::Image => ("img", true),
        Primitive::Button => ("button", false),
        Primitive::TextInput => ("input", true),
        Primitive::TextArea => ("textarea", false),
        Primitive::Toggle => ("input", true),
        Primitive::Slider => ("input", true),
        Primitive::Canvas => ("canvas", false),
        Primitive::PlatformView => ("div", false),
        Primitive::Text => ("span", false),
    }
}

struct Renderer<'a> {
    tree: &'a TreeMirror,
    limits: SsrLimits,
    nodes: usize,
    html: String,
    activation: Vec<ActivationEntry>,
}

impl Renderer<'_> {
    fn render_node(&mut self, id: NodeId, depth: usize) -> Result<(), SsrError> {
        if depth > self.limits.max_depth {
            return Err(SsrError::DepthLimitExceeded);
        }
        self.nodes += 1;
        if self.nodes > self.limits.max_nodes {
            return Err(SsrError::NodeLimitExceeded);
        }
        let node = self.tree.node(id).ok_or(SsrError::MissingNode(id))?;
        if bool_property(&node, PropertyId::HIDDEN)? == Some(true) {
            return Ok(());
        }
        if node.kind == NodeKind::Text {
            self.html.push_str("<!--volang-text:");
            write!(self.html, "{}:{}", id.index(), id.generation())
                .expect("writing to String cannot fail");
            self.html.push_str("-->");
            escape_text(&mut self.html, &node.text);
            return self.check_html();
        }
        let NodeKind::Element(primitive) = node.kind else {
            return Ok(());
        };
        let (tag, void) = tag(primitive);
        self.html.push('<');
        self.html.push_str(tag);
        attribute(
            &mut self.html,
            "data-volang-node",
            &format!("{}:{}", id.index(), id.generation()),
        );
        attribute(
            &mut self.html,
            "data-volang-primitive",
            match primitive {
                Primitive::Root => "root",
                Primitive::Fragment => "fragment",
                Primitive::Box => "box",
                Primitive::Row => "row",
                Primitive::Column => "column",
                Primitive::Stack => "stack",
                Primitive::Grid => "grid",
                Primitive::Scroll => "scroll",
                Primitive::Image => "image",
                Primitive::Button => "button",
                Primitive::TextInput => "text-input",
                Primitive::TextArea => "text-area",
                Primitive::Toggle => "toggle",
                Primitive::Slider => "slider",
                Primitive::Canvas => "canvas",
                Primitive::PlatformView => "platform-view",
                Primitive::Text => "text",
            },
        );
        if primitive == Primitive::Toggle {
            attribute(&mut self.html, "type", "checkbox");
        } else if primitive == Primitive::Slider {
            attribute(&mut self.html, "type", "range");
        }
        for (property, name) in [
            (PropertyId::ROLE, "role"),
            (PropertyId::ACCESSIBLE_NAME, "aria-label"),
            (PropertyId::ACCESSIBLE_DESCRIPTION, "aria-description"),
            (PropertyId::PLACEHOLDER, "placeholder"),
            (PropertyId::TEST_ID, "data-testid"),
            (PropertyId::CURRENT, "aria-current"),
            (PropertyId::SOURCE, "src"),
            (PropertyId::CONTENT_TYPE, "data-volang-content-type"),
            (PropertyId::GRAPHICS_PROGRAM, "data-volang-graphics"),
            (PropertyId::MEDIA_STATE, "data-volang-media-state"),
            (PropertyId::POSTER, "poster"),
        ] {
            if let Some(value) = text_property(&node, property)? {
                attribute(&mut self.html, name, value);
            }
        }
        for (property, name) in [
            (PropertyId::VALUE, "value"),
            (PropertyId::MIN_VALUE, "min"),
            (PropertyId::MAX_VALUE, "max"),
            (PropertyId::STEP_VALUE, "step"),
        ] {
            if let Some(value) = scalar_property(&node, property)? {
                attribute(&mut self.html, name, &value);
            }
        }
        boolean_attribute(
            &mut self.html,
            "disabled",
            bool_property(&node, PropertyId::DISABLED)?,
        );
        boolean_attribute(
            &mut self.html,
            "required",
            bool_property(&node, PropertyId::REQUIRED)?,
        );
        boolean_attribute(
            &mut self.html,
            "checked",
            bool_property(&node, PropertyId::CHECKED)?,
        );
        aria_boolean(
            &mut self.html,
            "aria-invalid",
            bool_property(&node, PropertyId::INVALID)?,
        );
        aria_boolean(
            &mut self.html,
            "aria-selected",
            bool_property(&node, PropertyId::SELECTED)?,
        );
        aria_boolean(
            &mut self.html,
            "aria-expanded",
            bool_property(&node, PropertyId::EXPANDED)?,
        );
        aria_boolean(
            &mut self.html,
            "aria-pressed",
            bool_property(&node, PropertyId::PRESSED)?,
        );
        aria_boolean(
            &mut self.html,
            "aria-hidden",
            bool_property(&node, PropertyId::ACCESSIBILITY_HIDDEN)?,
        );
        if let Some(focusable) = bool_property(&node, PropertyId::FOCUSABLE)? {
            attribute(
                &mut self.html,
                "tabindex",
                if focusable { "0" } else { "-1" },
            );
        }
        let node_style = style(&node)?;
        if !node_style.is_empty() {
            attribute(&mut self.html, "style", &node_style);
        }
        if !node.listeners.is_empty() {
            if self.activation.len() >= self.limits.max_activation_entries {
                return Err(SsrError::ActivationLimitExceeded);
            }
            self.activation.push(ActivationEntry {
                node: id,
                events: node.listeners.keys().copied().collect(),
            });
        }
        self.html.push('>');
        if primitive == Primitive::TextArea {
            if let Some(value) = text_property(&node, PropertyId::VALUE)? {
                escape_text(&mut self.html, value);
            }
        }
        for child in node.children {
            self.render_node(child, depth + 1)?;
        }
        if !void {
            self.html.push_str("</");
            self.html.push_str(tag);
            self.html.push('>');
        }
        self.check_html()
    }

    fn check_html(&self) -> Result<(), SsrError> {
        if self.html.len() > self.limits.max_html_bytes {
            Err(SsrError::HtmlLimitExceeded)
        } else {
            Ok(())
        }
    }
}

pub fn render_document(
    tree: &TreeMirror,
    metadata: &DocumentMetadata,
    limits: SsrLimits,
) -> Result<RenderedDocument, SsrError> {
    validate(metadata, limits)?;
    let mut renderer = Renderer {
        tree,
        limits,
        nodes: 0,
        html: String::new(),
        activation: Vec::new(),
    };
    renderer.html.push_str("<!doctype html><html");
    attribute(&mut renderer.html, "lang", &metadata.language);
    attribute(&mut renderer.html, "dir", &metadata.direction);
    renderer.html.push_str("><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">");
    renderer.html.push_str("<title>");
    escape_text(&mut renderer.html, &metadata.title);
    renderer.html.push_str("</title>");
    if !metadata.description.is_empty() {
        renderer
            .html
            .push_str("<meta name=\"description\" content=\"");
        escape_attribute(&mut renderer.html, &metadata.description);
        renderer.html.push_str("\">");
    }
    if let Some(canonical) = &metadata.canonical_url {
        renderer.html.push_str("<link rel=\"canonical\" href=\"");
        escape_attribute(&mut renderer.html, canonical);
        renderer.html.push_str("\">");
    }
    if let Some(color) = &metadata.theme_color {
        renderer
            .html
            .push_str("<meta name=\"theme-color\" content=\"");
        escape_attribute(&mut renderer.html, color);
        renderer.html.push_str("\">");
    }
    for asset in &metadata.assets {
        renderer.html.push_str("<link rel=\"");
        escape_attribute(
            &mut renderer.html,
            if asset.kind == "style" {
                "stylesheet"
            } else {
                &asset.kind
            },
        );
        renderer.html.push_str("\" href=\"");
        escape_attribute(&mut renderer.html, &asset.href);
        renderer.html.push('"');
        if let Some(integrity) = &asset.integrity {
            attribute(&mut renderer.html, "integrity", integrity);
            attribute(&mut renderer.html, "crossorigin", "anonymous");
        }
        renderer.html.push('>');
    }
    renderer
        .html
        .push_str("</head><body><div id=\"volang-root\" data-volang-revision=\"");
    write!(renderer.html, "{}", tree.revision()).expect("writing to String cannot fail");
    renderer.html.push_str("\">");
    let root = tree
        .node(tree.root())
        .ok_or(SsrError::MissingNode(tree.root()))?;
    for child in root.children {
        renderer.render_node(child, 1)?;
    }
    renderer.html.push_str("</div></body></html>");
    renderer.check_html()?;
    Ok(RenderedDocument {
        html: renderer.html,
        activation: renderer.activation,
        revision: tree.revision(),
    })
}

pub fn stream_document(
    document: &RenderedDocument,
    max_chunk_bytes: usize,
) -> Result<Vec<String>, SsrError> {
    if max_chunk_bytes == 0 {
        return Err(SsrError::InvalidLimits);
    }
    let mut chunks = Vec::new();
    let mut start = 0;
    while start < document.html.len() {
        let mut end = core::cmp::min(start + max_chunk_bytes, document.html.len());
        while end > start && !document.html.is_char_boundary(end) {
            end -= 1;
        }
        if end == start {
            end = document.html[start..]
                .char_indices()
                .nth(1)
                .map_or(document.html.len(), |(offset, _)| start + offset);
        }
        chunks.push(document.html[start..end].to_string());
        start = end;
    }
    Ok(chunks)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use vo_ui_core::{HandlerId, Listener, Property};
    use vo_ui_protocol::{Mutation, MutationBatch, ProtocolLimits};

    #[test]
    fn semantic_html_is_escaped_and_activation_is_selective() {
        let root = NodeId::new(0, 1);
        let button = NodeId::new(1, 1);
        let text = NodeId::new(2, 1);
        let mut tree = TreeMirror::new(7, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            7,
            1,
            vec![
                Mutation::Create {
                    id: button,
                    kind: NodeKind::Element(Primitive::Button),
                },
                Mutation::SetProperty {
                    id: button,
                    property: Property::new(PropertyId::ACCESSIBLE_NAME, "Save <draft>"),
                },
                Mutation::SetProperty {
                    id: button,
                    property: Property::new(PropertyId::FOCUSABLE, true),
                },
                Mutation::Listen {
                    id: button,
                    listener: Listener::new(EventType::CLICK, HandlerId::new(3, 1)),
                },
                Mutation::Create {
                    id: text,
                    kind: NodeKind::Text,
                },
                Mutation::SetText {
                    id: text,
                    text: "Save & continue".into(),
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
        let metadata = DocumentMetadata {
            title: "Account <settings>".into(),
            description: "Useful before Wasm".into(),
            assets: vec![AssetLink {
                href: "/site.css".into(),
                kind: "style".into(),
                integrity: None,
            }],
            ..DocumentMetadata::default()
        };
        let rendered = render_document(&tree, &metadata, SsrLimits::default()).unwrap();
        assert!(rendered.html.contains("Account &lt;settings&gt;"));
        assert!(rendered.html.contains("Save &amp; continue"));
        assert!(rendered.html.contains("aria-label=\"Save &lt;draft&gt;\""));
        assert!(rendered.html.contains("tabindex=\"0\""));
        assert!(rendered
            .html
            .contains("rel=\"stylesheet\" href=\"/site.css\""));
        assert_eq!(rendered.activation.len(), 1);
        assert_eq!(rendered.activation[0].node, button);
        let chunks = stream_document(&rendered, 17).unwrap();
        assert_eq!(chunks.concat(), rendered.html);
        assert!(chunks
            .iter()
            .all(|chunk| chunk.len() <= 17 || chunk.chars().count() == 1));
    }

    #[test]
    fn hidden_content_and_oversized_documents_fail_closed() {
        let root = NodeId::new(0, 1);
        let hidden = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(8, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            8,
            1,
            vec![
                Mutation::Create {
                    id: hidden,
                    kind: NodeKind::Element(Primitive::Text),
                },
                Mutation::SetProperty {
                    id: hidden,
                    property: Property::new(PropertyId::HIDDEN, true),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: hidden,
                    before: None,
                },
            ],
        ))
        .unwrap();
        let rendered =
            render_document(&tree, &DocumentMetadata::default(), SsrLimits::default()).unwrap();
        assert!(!rendered.html.contains("data-volang-node=\"1:1\""));

        tree.apply(&MutationBatch::new(
            8,
            2,
            vec![
                Mutation::RemoveProperty {
                    id: hidden,
                    property: PropertyId::HIDDEN,
                },
                Mutation::SetProperty {
                    id: hidden,
                    property: Property::new(PropertyId::ACCESSIBILITY_HIDDEN, true),
                },
            ],
        ))
        .unwrap();
        let rendered =
            render_document(&tree, &DocumentMetadata::default(), SsrLimits::default()).unwrap();
        assert!(rendered.html.contains("data-volang-node=\"1:1\""));
        assert!(rendered.html.contains("aria-hidden=\"true\""));
        assert_eq!(
            render_document(
                &tree,
                &DocumentMetadata::default(),
                SsrLimits {
                    max_html_bytes: 1,
                    ..SsrLimits::default()
                },
            ),
            Err(SsrError::HtmlLimitExceeded)
        );
    }

    #[test]
    fn layout_primitives_and_argb_colors_match_the_live_dom() {
        let root = NodeId::new(0, 1);
        let row = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(9, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            9,
            1,
            vec![
                Mutation::Create {
                    id: row,
                    kind: NodeKind::Element(Primitive::Row),
                },
                Mutation::SetProperty {
                    id: row,
                    property: Property::new(PropertyId::BACKGROUND, Value::Color(0x8011_2233)),
                },
                Mutation::SetProperty {
                    id: row,
                    property: Property::new(PropertyId::FOREGROUND, Value::Color(0xff44_5566)),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: row,
                    before: None,
                },
            ],
        ))
        .unwrap();
        let rendered =
            render_document(&tree, &DocumentMetadata::default(), SsrLimits::default()).unwrap();
        assert!(rendered.html.contains(
            "style=\"min-width:0;min-height:0;display:flex;flex-direction:row;background-color:#11223380;color:#445566ff;\""
        ));
    }

    #[test]
    fn slider_renders_numeric_range_semantics_before_activation() {
        let root = NodeId::new(0, 1);
        let slider = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(10, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            10,
            1,
            vec![
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
                    property: Property::new(PropertyId::STEP_VALUE, 0.5_f64),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: slider,
                    before: None,
                },
            ],
        ))
        .unwrap();
        let rendered =
            render_document(&tree, &DocumentMetadata::default(), SsrLimits::default()).unwrap();
        assert!(rendered.html.contains("type=\"range\""));
        assert!(rendered.html.contains("aria-label=\"Quality\""));
        assert!(rendered.html.contains("value=\"42.5\""));
        assert!(rendered.html.contains("min=\"0\""));
        assert!(rendered.html.contains("max=\"100\""));
        assert!(rendered.html.contains("step=\"0.5\""));
    }
}
