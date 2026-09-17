//! Build-time CommonMark parsing. The browser consumes bounded semantic nodes,
//! so document content and a Markdown parser do not enter Studio's main image.
use anyhow::{bail, Context, Result};
use pulldown_cmark::{Alignment, CodeBlockKind, Event, Options, Parser, Tag, TagEnd};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

const MAX_NODES: usize = 8192;
const MAX_DEPTH: usize = 64;

#[derive(Default, Debug, Serialize)]
pub(super) struct Node {
    pub tag: String,
    #[serde(skip_serializing_if = "String::is_empty")]
    pub text: String,
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub attrs: BTreeMap<String, String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub children: Vec<Node>,
}

impl Node {
    fn new(tag: &str) -> Self {
        Self {
            tag: tag.into(),
            ..Self::default()
        }
    }
    fn text(text: impl Into<String>) -> Self {
        Self {
            tag: "#text".into(),
            text: text.into(),
            ..Self::default()
        }
    }
    fn attr(&mut self, name: &str, value: impl Into<String>) {
        self.attrs.insert(name.into(), value.into());
    }
    fn push(&mut self, next: Node) {
        if next.tag == "#text" {
            if let Some(previous) = self.children.last_mut().filter(|node| node.tag == "#text") {
                previous.text.push_str(&next.text);
                return;
            }
        }
        self.children.push(next);
    }
    fn plain_text(&self) -> String {
        let mut value = self.text.clone();
        if self.tag == "br" {
            value.push(' ');
        }
        for child in &self.children {
            value.push_str(&child.plain_text());
        }
        value
    }
}

#[derive(Serialize)]
pub(super) struct Document {
    pub version: u32,
    #[serde(rename = "headingID")]
    pub heading_id: String,
    #[serde(skip)]
    heading_text: String,
    pub nodes: Vec<Node>,
}

impl Document {
    // Search uses the same semantic source as rendering. Separate block text so
    // neighbouring paragraphs, table cells and code blocks cannot merge words.
    pub(super) fn search_text(&self) -> String {
        fn append(node: &Node, text: &mut String) {
            text.push_str(&node.text);
            for child in &node.children {
                append(child, text);
            }
            if matches!(
                node.tag.as_str(),
                "p" | "h1"
                    | "h2"
                    | "h3"
                    | "h4"
                    | "h5"
                    | "h6"
                    | "pre"
                    | "li"
                    | "th"
                    | "td"
                    | "blockquote"
                    | "br"
            ) {
                text.push(' ');
            }
        }
        let mut text = self.heading_text.clone();
        text.push(' ');
        for node in &self.nodes {
            append(node, &mut text);
        }
        text.split_whitespace().collect::<Vec<_>>().join(" ")
    }
}

struct Frame {
    node: Node,
    end: Option<TagEnd>,
    alignments: Vec<Alignment>,
}

pub(super) fn parse(
    source: &str,
    source_path: &str,
    pages: &BTreeMap<String, String>,
) -> Result<Document> {
    if source.len() > 2 * 1024 * 1024 || source.contains('\0') {
        bail!("document source must fit 2 MiB and contain no NUL characters");
    }
    let mut stack = vec![Frame {
        node: Node::new("root"),
        end: None,
        alignments: vec![],
    }];
    let mut headings = BTreeSet::new();
    let mut suffixes = BTreeMap::<String, usize>::new();
    let mut count = 0;
    let options =
        Options::ENABLE_TABLES | Options::ENABLE_STRIKETHROUGH | Options::ENABLE_TASKLISTS;
    for event in Parser::new_ext(source, options) {
        count += 1;
        if count > MAX_NODES * 3 {
            bail!("document exceeds its parsing event budget");
        }
        match event {
            Event::Start(tag) => {
                if stack.len() >= MAX_DEPTH {
                    bail!("document nesting exceeds {MAX_DEPTH}");
                }
                let mut alignments = vec![];
                let node = match &tag {
                    Tag::Paragraph => Node::new("p"),
                    Tag::Heading { level, .. } => Node::new(&level.to_string()),
                    Tag::BlockQuote(_) => Node::new("blockquote"),
                    Tag::CodeBlock(kind) => {
                        let mut node = Node::new("pre");
                        if let CodeBlockKind::Fenced(info) = kind {
                            if let Some(language) = info.split_whitespace().next() {
                                if language.len() > 128 {
                                    bail!("code language label exceeds 128 bytes");
                                }
                                node.attr("data-language", language);
                            }
                        }
                        node
                    }
                    // Authored raw HTML remains visible source text. Semantic
                    // document content never delegates DOM ownership to markup.
                    Tag::HtmlBlock => Node::new("pre"),
                    Tag::List(start) => {
                        let mut node = Node::new(if start.is_some() { "ol" } else { "ul" });
                        if let Some(start) = start {
                            let value = i32::try_from(*start)
                                .context("ordered list start exceeds its native integer range")?;
                            node.attr("start", value.to_string());
                        }
                        node
                    }
                    Tag::Item => Node::new("li"),
                    Tag::Table(columns) => {
                        alignments.clone_from(columns);
                        Node::new("table")
                    }
                    Tag::TableHead => Node::new("thead"),
                    Tag::TableRow => Node::new("tr"),
                    Tag::TableCell => {
                        let parent = &stack.last().unwrap().node;
                        let mut node = Node::new(if parent.tag == "thead" { "th" } else { "td" });
                        if node.tag == "th" {
                            node.attr("scope", "col");
                        }
                        let table = stack
                            .iter()
                            .rev()
                            .find(|frame| frame.node.tag == "table")
                            .context("table cell without table")?;
                        match table
                            .alignments
                            .get(parent.children.len())
                            .copied()
                            .unwrap_or(Alignment::None)
                        {
                            Alignment::Left => node.attr("class", "doc-align-start"),
                            Alignment::Center => node.attr("class", "doc-align-center"),
                            Alignment::Right => node.attr("class", "doc-align-end"),
                            Alignment::None => (),
                        }
                        node
                    }
                    Tag::Emphasis => Node::new("em"),
                    Tag::Strong => Node::new("strong"),
                    Tag::Strikethrough => Node::new("del"),
                    Tag::Link {
                        dest_url, title, ..
                    }
                    | Tag::Image {
                        dest_url, title, ..
                    } => {
                        let image = matches!(&tag, Tag::Image { .. });
                        let mut node = Node::new(if image { "img" } else { "a" });
                        node.attr(
                            if image { "src" } else { "href" },
                            link(dest_url, source_path, pages, image)?,
                        );
                        if !title.is_empty() {
                            node.attr("title", title.to_string());
                        }
                        node
                    }
                    other => bail!("unsupported enabled document syntax: {other:?}"),
                };
                stack.push(Frame {
                    node,
                    end: Some(tag.to_end()),
                    alignments,
                });
            }
            Event::End(end) => {
                let Frame {
                    mut node,
                    end: expected,
                    ..
                } = stack.pop().context("document end without start")?;
                if expected != Some(end) {
                    bail!("document syntax stack is unbalanced");
                }
                if node.tag.starts_with('h') && node.tag.len() == 2 {
                    let base = heading_id(&node.plain_text());
                    let next = suffixes.entry(base.clone()).or_default();
                    let mut id = base.clone();
                    while !headings.insert(id.clone()) {
                        *next += 1;
                        id = format!("{base}-{next}");
                    }
                    node.attr("id", id);
                }
                if node.tag == "img" {
                    node.attr("alt", node.plain_text());
                    node.children.clear();
                } else if node.tag == "pre" {
                    let mut code = Node::new("code");
                    code.children = std::mem::take(&mut node.children);
                    node.children.push(code);
                } else if node.tag == "thead" {
                    let mut row = Node::new("tr");
                    row.children = std::mem::take(&mut node.children);
                    node.children.push(row);
                } else if node.tag == "table" {
                    let mut rows = Node::new("tbody");
                    let mut children = vec![];
                    for child in std::mem::take(&mut node.children) {
                        if child.tag == "thead" {
                            children.push(child);
                        } else {
                            rows.children.push(child);
                        }
                    }
                    if !rows.children.is_empty() {
                        children.push(rows);
                    }
                    node.children = children;
                }
                stack
                    .last_mut()
                    .context("document lost its root")?
                    .node
                    .push(node);
            }
            event => {
                let node = match event {
                    Event::Text(value) | Event::Html(value) | Event::InlineHtml(value) => {
                        Node::text(value.into_string())
                    }
                    Event::Code(value) => {
                        let mut node = Node::new("code");
                        node.push(Node::text(value.into_string()));
                        node
                    }
                    Event::SoftBreak => Node::text("\n"),
                    Event::HardBreak => Node::new("br"),
                    Event::Rule => Node::new("hr"),
                    Event::TaskListMarker(checked) => {
                        let mut node = Node::new("input");
                        node.attr("type", "checkbox");
                        node.attr("disabled", "true");
                        node.attr("checked", checked.to_string());
                        node.attr(
                            "aria-label",
                            if checked { "Completed" } else { "Incomplete" },
                        );
                        node
                    }
                    other => bail!("unsupported enabled document event: {other:?}"),
                };
                stack
                    .last_mut()
                    .context("document lost its root")?
                    .node
                    .push(node);
            }
        }
    }
    if stack.len() != 1 {
        bail!("document contains an unfinished syntax node");
    }
    let mut nodes = stack.pop().unwrap().node.children;
    let (heading_id, heading_text) = if nodes.first().is_some_and(|node| node.tag == "h1") {
        let mut heading = nodes.remove(0);
        (
            heading.attrs.remove("id").unwrap_or_default(),
            heading.plain_text(),
        )
    } else {
        (String::new(), String::new())
    };
    fn validate(nodes: &[Node], depth: usize, count: &mut usize) -> Result<()> {
        if !nodes.is_empty() && depth >= MAX_DEPTH {
            bail!("document nesting exceeds {MAX_DEPTH}");
        }
        for node in nodes {
            *count += 1;
            if *count > MAX_NODES {
                bail!("document exceeds {MAX_NODES} semantic nodes");
            }
            for (name, value) in &node.attrs {
                let limit = if name == "id" { 1024 } else { 4096 };
                if value.len() > limit || value.contains('\0') {
                    bail!("document {name} exceeds its attribute text limit");
                }
            }
            validate(&node.children, depth + 1, count)?;
        }
        Ok(())
    }
    if heading_id.len() > 1024 {
        bail!("document heading ID exceeds 1024 bytes");
    }
    validate(&nodes, 0, &mut 0)?;
    Ok(Document {
        version: 1,
        heading_id,
        heading_text,
        nodes,
    })
}

fn heading_id(text: &str) -> String {
    let mut slug = String::new();
    for character in text.to_lowercase().chars() {
        if character.is_alphanumeric() || character == '_' || character == '-' {
            slug.push(character);
        } else if character.is_whitespace() {
            slug.push('-');
        }
    }
    if slug.is_empty() {
        slug.push_str("section");
    }
    format!("doc-{slug}")
}

fn link(href: &str, source: &str, pages: &BTreeMap<String, String>, image: bool) -> Result<String> {
    if href.len() > 4096 || href.chars().any(char::is_control) {
        bail!("document link exceeds its text limit");
    }
    let lower = href.to_ascii_lowercase();
    if lower.starts_with("https://")
        || lower.starts_with("http://")
        || (!image && lower.starts_with("mailto:"))
    {
        return Ok(href.into());
    }
    let (path_query, fragment) = href.split_once('#').unwrap_or((href, ""));
    let (path, query) = path_query
        .split_once('?')
        .map_or((path_query, String::new()), |(path, query)| {
            (path, format!("?{query}"))
        });
    if path.contains(':') || href.starts_with("//") || href.contains('\\') {
        bail!("unsupported document link: {href}");
    }
    let anchor = if fragment.is_empty() {
        String::new()
    } else {
        format!("#doc-{fragment}")
    };
    if path.is_empty() {
        return Ok(format!("{query}{anchor}"));
    }
    let mut parts = vec![];
    let relative = Path::new(source)
        .parent()
        .unwrap_or(Path::new(""))
        .join(path);
    for component in relative.components() {
        match component {
            Component::Normal(part) => parts.push(part.to_string_lossy().into_owned()),
            Component::CurDir => (),
            Component::ParentDir if !parts.is_empty() => {
                parts.pop();
            }
            _ => bail!("document link leaves the repository: {href}"),
        }
    }
    let path = parts.join("/");
    if !image {
        if let Some(id) = pages.get(&path) {
            return Ok(format!("/studio/docs/{id}{query}{anchor}"));
        }
    }
    let mut encoded = String::new();
    for byte in path.bytes() {
        if byte.is_ascii_alphanumeric() || b"/-_.~".contains(&byte) {
            encoded.push(char::from(byte));
        } else {
            use std::fmt::Write;
            write!(encoded, "%{byte:02X}").unwrap();
        }
    }
    let base = if image {
        "https://raw.githubusercontent.com/vo-lang/volang/main/"
    } else {
        "https://github.com/vo-lang/volang/blob/main/"
    };
    let fragment = if fragment.is_empty() {
        String::new()
    } else {
        format!("#{fragment}")
    };
    Ok(format!("{base}{encoded}{query}{fragment}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn commonmark_builds_semantic_nested_content_and_unique_headings() {
        let pages = BTreeMap::from([("lang/docs/other.md".into(), "other".into())]);
        let doc = parse("# Introduction\n\n## A & 中文\n\n## A & 中文\n\n3. **Strong** and *soft*\n   - nested `code`\n\n[Other](other.md#details)\n\n```vo\nprintln(\"<tag>\")\n```\n", "lang/docs/start.md", &pages).unwrap();
        assert_eq!(doc.heading_id, "doc-introduction");
        assert_eq!(doc.nodes[0].attrs["id"], "doc-a--中文");
        assert_eq!(doc.nodes[1].attrs["id"], "doc-a--中文-1");
        assert_eq!(doc.nodes[2].tag, "ol");
        assert_eq!(doc.nodes[2].attrs["start"], "3");
        let json = serde_json::to_string(&doc).unwrap();
        assert!(json.contains("/studio/docs/other#doc-details"));
        assert!(json.contains("strong") && json.contains("em") && json.contains("println"));
        assert_eq!(doc.nodes.last().unwrap().children[0].tag, "code");
    }

    #[test]
    fn tables_have_explicit_sections_and_native_header_cells() {
        let doc = parse(
            "# Table\n\n| Name | Value |\n| :--- | ---: |\n| Ada | 42 |\n",
            "guide.md",
            &BTreeMap::new(),
        )
        .unwrap();
        let table = &doc.nodes[0];
        assert_eq!(table.children[0].tag, "thead");
        assert_eq!(table.children[0].children[0].tag, "tr");
        assert_eq!(
            table.children[0].children[0].children[1].attrs["scope"],
            "col"
        );
        assert_eq!(table.children[1].tag, "tbody");
        assert_eq!(
            table.children[1].children[0].children[1].attrs["class"],
            "doc-align-end"
        );
    }

    #[test]
    fn raw_html_remains_text_and_links_resolve_inside_known_documents() {
        let doc = parse(
            "# Test\n\n<script>literal()</script>\n\n- [x] finished\n",
            "guide.md",
            &BTreeMap::new(),
        )
        .unwrap();
        assert_eq!(doc.nodes[0].tag, "pre");
        assert_eq!(doc.nodes[0].children[0].children[0].tag, "#text");
        assert!(doc.nodes[0].plain_text().contains("<script>"));
        for href in ["javascript:wrong", "//example.com", "../../outside.md"] {
            assert!(link(href, "guide.md", &BTreeMap::new(), false).is_err());
        }
        assert!(parse(
            &format!("# Test\n\n{}deep", "> ".repeat(65)),
            "guide.md",
            &BTreeMap::new()
        )
        .is_err());
    }
}
