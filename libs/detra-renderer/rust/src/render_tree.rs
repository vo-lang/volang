//! Renderer - paints LayoutNode tree using egui.

use std::cell::RefCell;
use std::collections::HashMap;

use eframe::egui::{self, Color32, FontId, Align2, Stroke, Rect, pos2, vec2};
use detra_renderable::{RuntimeNode, Value};

use super::layout::LayoutNode;
use super::PENDING_ACTIONS;
use super::theme::colors;

thread_local! {
    static INPUT_BUFFERS: RefCell<HashMap<u64, String>> = RefCell::new(HashMap::new());
}

pub fn render(ctx: &egui::Context, root: &LayoutNode) {
    let painter = ctx.layer_painter(egui::LayerId::background());
    paint_static(&painter, root);
    
    let mut id = 0u64;
    render_widgets(ctx, root, &mut id);
}

fn node_rect(node: &LayoutNode) -> Rect {
    Rect::from_min_size(pos2(node.x, node.y), vec2(node.width, node.height))
}

fn paint_static(painter: &egui::Painter, node: &LayoutNode) {
    let rect = node_rect(node);
    let rt = &node.runtime;
    
    match rt.kind.as_str() {
        "Column" | "Row" => {
            if let Some(bg) = style_color(rt.get_string("style")) {
                painter.rect_filled(rect, 0.0, bg);
            }
        }
        "Text" => {
            let text = rt.get_string("text").unwrap_or("");
            let size = rt.get_float("size").unwrap_or(14.0) as f32;
            let color = parse_color(rt.get_string("color")).unwrap_or(colors::TEXT_PRIMARY);
            let bold = rt.get_bool("bold").unwrap_or(false);
            let monospace = rt.get_string("font") == Some("monospace");
            
            let family = if monospace {
                egui::FontFamily::Monospace
            } else if bold {
                egui::FontFamily::Name("Bold".into())
            } else {
                egui::FontFamily::Proportional
            };
            
            let galley = painter.layout_no_wrap(
                text.to_string(),
                FontId::new(size, family),
                color,
            );
            painter.galley(rect.min, galley, color);
        }
        "Divider" => {
            if rt.get_bool("vertical").unwrap_or(false) {
                painter.line_segment([pos2(rect.center().x, rect.min.y), pos2(rect.center().x, rect.max.y)], Stroke::new(1.0, colors::DIVIDER));
            } else {
                painter.line_segment([pos2(rect.min.x, rect.center().y), pos2(rect.max.x, rect.center().y)], Stroke::new(1.0, colors::DIVIDER));
            }
        }
        "Image" => {
            let src = rt.get_string("src").unwrap_or("");
            painter.rect_filled(rect, 0.0, colors::INPUT_BG);
            painter.text(rect.center(), Align2::CENTER_CENTER, format!("[{}]", src), FontId::proportional(10.0), colors::TEXT_SECONDARY);
        }
        _ => {}
    }
    
    for child in &node.children {
        paint_static(painter, child);
    }
}

fn render_widgets(ctx: &egui::Context, node: &LayoutNode, id: &mut u64) {
    let rect = node_rect(node);
    let rt = &node.runtime;
    
    match rt.kind.as_str() {
        "Button" => {
            let widget_id = next_id(rt, id);
            let text = rt.get_string("text").unwrap_or("");
            let active = rt.get_bool("active").unwrap_or(false);
            let style = rt.get_string("style").unwrap_or("");
            
            egui::Area::new(egui::Id::new(widget_id))
                .fixed_pos(rect.min)
                .order(egui::Order::Middle)
                .show(ctx, |ui| {
                    ui.set_min_size(rect.size());
                    let (bg_normal, bg_hover, text_color, rounding) = button_style(style, active);
                    
                    let response = ui.allocate_rect(rect, egui::Sense::click());
                    let bg = if !active && response.hovered() { bg_hover } else { bg_normal };
                    
                    ui.painter().rect_filled(rect, rounding, bg);
                    
                    let (text_pos, align) = if style == "treeitem" {
                        (pos2(rect.min.x + 4.0, rect.center().y), Align2::LEFT_CENTER)
                    } else {
                        (rect.center(), Align2::CENTER_CENTER)
                    };
                    let font_size = if style == "treeitem" { 12.0 } else { 14.0 };
                    ui.painter().text(text_pos, align, text, FontId::proportional(font_size), text_color);
                    
                    if response.clicked() {
                        if let Some(action) = rt.events.get("onClick") {
                            PENDING_ACTIONS.with(|c| c.borrow_mut().push(action.clone()));
                        }
                    }
                });
        }
        "Input" => {
            let widget_id = next_id(rt, id);
            let value = rt.get_string("value").unwrap_or("").to_string();
            let placeholder = rt.get_string("placeholder").unwrap_or("");
            let multiline = rt.get_bool("multiline").unwrap_or(false);
            
            INPUT_BUFFERS.with(|cell| {
                let mut buffers = cell.borrow_mut();
                let buffer = buffers.entry(widget_id).or_insert_with(|| value.clone());
                
                // Sync from state when value changed externally
                if *buffer != value {
                    *buffer = value.clone();
                }
                let buf = buffer.clone();
                
                egui::Area::new(egui::Id::new(widget_id))
                    .fixed_pos(rect.min)
                    .order(egui::Order::Middle)
                    .show(ctx, |ui| {
                        ui.set_min_size(rect.size());
                        let mut text = buf;
                        let edit = if multiline {
                            egui::TextEdit::multiline(&mut text).hint_text(placeholder).font(FontId::monospace(14.0)).frame(true)
                        } else {
                            egui::TextEdit::singleline(&mut text).hint_text(placeholder).frame(true)
                        };
                        if ui.add_sized(rect.size(), edit).changed() {
                            buffers.insert(widget_id, text.clone());
                            if let Some(action) = rt.events.get("onChange") {
                                let mut a = action.clone();
                                a.args.insert("value".to_string(), Value::String(text));
                                PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                            }
                        }
                    });
            });
        }
        _ => {}
    }
    
    for child in &node.children {
        render_widgets(ctx, child, id);
    }
}

fn next_id(node: &RuntimeNode, counter: &mut u64) -> u64 {
    if let Some(key) = &node.key {
        use std::hash::{Hash, Hasher};
        let mut h = std::collections::hash_map::DefaultHasher::new();
        format!("{:?}", key).hash(&mut h);
        h.finish()
    } else {
        *counter += 1;
        *counter
    }
}

fn style_color(style: Option<&str>) -> Option<Color32> {
    match style? {
        "sidebar" => Some(colors::SIDEBAR_BG),
        "panel" => Some(colors::SIDEBAR_BG),
        "menubar" => Some(colors::MENUBAR_BG),
        "tabbar" => Some(colors::TABBAR_BG),
        "statusbar" => Some(colors::STATUSBAR_BG),
        "editor" => Some(colors::EDITOR_BG),
        "activitybar" => Some(colors::ACTIVITY_BAR_BG),
        _ => None,
    }
}

fn button_style(style: &str, active: bool) -> (Color32, Color32, Color32, f32) {
    match style {
        "icon" => {
            let bg = if active { colors::LIST_SELECTED } else { Color32::TRANSPARENT };
            let hover = colors::LIST_HOVER;
            (bg, hover, colors::TEXT_PRIMARY, 4.0)
        }
        "tab" => {
            let bg = if active { colors::EDITOR_BG } else { Color32::TRANSPARENT };
            let hover = colors::LIST_HOVER;
            (bg, hover, colors::TEXT_PRIMARY, 0.0)
        }
        "treeitem" => {
            let bg = if active { colors::LIST_SELECTED } else { Color32::TRANSPARENT };
            let hover = colors::LIST_HOVER;
            (bg, hover, colors::TEXT_PRIMARY, 2.0)
        }
        _ => {
            let bg = if active { colors::BUTTON_BG } else { colors::BUTTON_SECONDARY };
            let hover = colors::BUTTON_SECONDARY_HOVER;
            let text = if active { Color32::WHITE } else { colors::TEXT_PRIMARY };
            (bg, hover, text, 4.0)
        }
    }
}

fn parse_color(s: Option<&str>) -> Option<Color32> {
    match s? {
        "white" => Some(Color32::WHITE),
        "black" => Some(Color32::BLACK),
        "gray" | "grey" => Some(colors::TEXT_SECONDARY),
        "red" => Some(Color32::from_rgb(244, 67, 54)),
        "green" => Some(Color32::from_rgb(76, 175, 80)),
        "blue" => Some(colors::TEXT_LINK),
        "primary" => Some(colors::TEXT_PRIMARY),
        "secondary" => Some(colors::TEXT_SECONDARY),
        "disabled" => Some(colors::TEXT_DISABLED),
        "keyword" => Some(colors::SYNTAX_KEYWORD),
        "string" => Some(colors::SYNTAX_STRING),
        "number" => Some(colors::SYNTAX_NUMBER),
        "comment" => Some(colors::SYNTAX_COMMENT),
        "function" => Some(colors::SYNTAX_FUNCTION),
        "type" => Some(colors::SYNTAX_TYPE),
        "variable" => Some(colors::SYNTAX_VARIABLE),
        s if s.starts_with('#') && s.len() == 7 => {
            let r = u8::from_str_radix(&s[1..3], 16).ok()?;
            let g = u8::from_str_radix(&s[3..5], 16).ok()?;
            let b = u8::from_str_radix(&s[5..7], 16).ok()?;
            Some(Color32::from_rgb(r, g, b))
        }
        _ => None,
    }
}
