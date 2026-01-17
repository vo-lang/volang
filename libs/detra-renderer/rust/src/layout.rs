//! Layout engine - converts RuntimeNode to positioned nodes using taffy flexbox.

use std::collections::HashMap;
use taffy::prelude::*;
use detra_renderable::RuntimeNode;

#[derive(Debug, Clone)]
pub struct LayoutNode {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub runtime: RuntimeNode,
    pub children: Vec<LayoutNode>,
}

pub fn compute_layout(root: &RuntimeNode, width: f32, height: f32) -> LayoutNode {
    let mut taffy = TaffyTree::new();
    let mut nodes: HashMap<NodeId, RuntimeNode> = HashMap::new();
    
    let root_id = build_tree(&mut taffy, root, &mut nodes);
    taffy.compute_layout(root_id, Size {
        width: AvailableSpace::Definite(width),
        height: AvailableSpace::Definite(height),
    }).unwrap();
    
    extract_layout(&taffy, root_id, &nodes, 0.0, 0.0)
}

fn build_tree(taffy: &mut TaffyTree, node: &RuntimeNode, nodes: &mut HashMap<NodeId, RuntimeNode>) -> NodeId {
    let children: Vec<NodeId> = node.children.iter()
        .map(|c| build_tree(taffy, c, nodes))
        .collect();
    
    let id = taffy.new_with_children(node_style(node), &children).unwrap();
    nodes.insert(id, node.clone());
    id
}

fn node_style(node: &RuntimeNode) -> Style {
    let mut s = Style {
        display: Display::Flex,
        flex_direction: match node.kind.as_str() {
            "Column" => FlexDirection::Column,
            _ => FlexDirection::Row,
        },
        ..Default::default()
    };
    
    // Size
    let fill = node.get_bool("fill").unwrap_or(false);
    if let Some(w) = node.get_float("width") {
        s.size.width = Dimension::Length(w as f32);
    } else if fill {
        s.size.width = Dimension::Percent(1.0);
    }
    if let Some(h) = node.get_float("height") {
        s.size.height = Dimension::Length(h as f32);
    } else if fill {
        s.size.height = Dimension::Percent(1.0);
    }
    
    // Flex
    if let Some(flex) = node.get_float("flex") {
        s.flex_grow = flex as f32;
        s.flex_shrink = 1.0;
        s.flex_basis = Dimension::Length(0.0);
    }
    
    // Padding & Spacing
    if let Some(p) = node.get_float("padding") {
        let p = LengthPercentage::Length(p as f32);
        s.padding = Rect { left: p, right: p, top: p, bottom: p };
    }
    if let Some(g) = node.get_float("spacing") {
        s.gap = Size { width: LengthPercentage::Length(g as f32), height: LengthPercentage::Length(g as f32) };
    }
    
    // Widget-specific sizing
    apply_widget_size(node, &mut s);
    s
}

fn apply_widget_size(node: &RuntimeNode, s: &mut Style) {
    match node.kind.as_str() {
        "Text" => {
            let text = node.get_string("text").unwrap_or("");
            let size = node.get_float("size").unwrap_or(14.0) as f32;
            let width = (text.len() as f32 * size * 0.6).max(10.0);
            if s.size.width == Dimension::Auto {
                s.size.width = Dimension::Length(width);
            }
            if s.size.height == Dimension::Auto {
                s.size.height = Dimension::Length(size * 1.4);
            }
        }
        "Button" => {
            let text = node.get_string("text").unwrap_or("");
            let style = node.get_string("style").unwrap_or("");
            let size = node.get_float("size").unwrap_or(14.0) as f32;
            
            if style == "icon" {
                if s.size.width == Dimension::Auto {
                    s.size.width = Dimension::Length(36.0);
                }
                if s.size.height == Dimension::Auto {
                    s.size.height = Dimension::Length(36.0);
                }
            } else if style == "tab" {
                let width = (text.len() as f32 * size * 0.6 + 16.0).max(60.0);
                if s.size.width == Dimension::Auto {
                    s.size.width = Dimension::Length(width);
                }
                if s.size.height == Dimension::Auto {
                    s.size.height = Dimension::Length(28.0);
                }
            } else if style == "treeitem" {
                if s.size.width == Dimension::Auto {
                    s.size.width = Dimension::Percent(1.0);
                }
                if s.size.height == Dimension::Auto {
                    s.size.height = Dimension::Length(22.0);
                }
            } else {
                let width = (text.len() as f32 * size * 0.6 + 16.0).max(40.0);
                if s.size.width == Dimension::Auto {
                    s.size.width = Dimension::Length(width);
                }
                if s.size.height == Dimension::Auto {
                    s.size.height = Dimension::Length(size * 1.4 + 8.0);
                }
            }
        }
        "Input" => {
            let multiline = node.get_bool("multiline").unwrap_or(false);
            if s.size.height == Dimension::Auto {
                s.size.height = Dimension::Length(if multiline { 100.0 } else { 28.0 });
            }
            if s.size.width == Dimension::Auto {
                s.flex_grow = 1.0;
            }
        }
        "Divider" => {
            if node.get_bool("vertical").unwrap_or(false) {
                s.size.width = Dimension::Length(1.0);
            } else {
                s.size.height = Dimension::Length(1.0);
            }
        }
        "Spacer" => {
            if let Some(size) = node.get_float("size") {
                s.size.width = Dimension::Length(size as f32);
                s.size.height = Dimension::Length(size as f32);
            } else {
                s.flex_grow = 1.0;
            }
        }
        _ => {}
    }
}

fn extract_layout(
    taffy: &TaffyTree,
    id: NodeId,
    nodes: &HashMap<NodeId, RuntimeNode>,
    px: f32,
    py: f32,
) -> LayoutNode {
    let l = taffy.layout(id).unwrap();
    let x = px + l.location.x;
    let y = py + l.location.y;
    
    LayoutNode {
        x,
        y,
        width: l.size.width,
        height: l.size.height,
        runtime: nodes.get(&id).unwrap().clone(),
        children: taffy.children(id).unwrap().iter()
            .map(|&cid| extract_layout(taffy, cid, nodes, x, y))
            .collect(),
    }
}
