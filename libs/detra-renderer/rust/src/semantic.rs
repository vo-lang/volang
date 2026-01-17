//! Semantic Component Renderer - renders Detra semantic components using egui native widgets.

use std::cell::RefCell;
use std::collections::HashMap;

use eframe::egui::{self, Color32, FontId, RichText, Sense, ScrollArea, TextEdit};
use detra_renderable::{RuntimeNode, Value};

use super::PENDING_ACTIONS;
use super::theme::colors;

thread_local! {
    static EDITOR_BUFFERS: RefCell<HashMap<String, String>> = RefCell::new(HashMap::new());
    static TREE_EXPANDED: RefCell<HashMap<String, bool>> = RefCell::new(HashMap::new());
}

/// Main entry point for semantic rendering
pub fn render(ctx: &egui::Context, root: &RuntimeNode) {
    // Find AppShell and render it
    if root.kind == "AppShell" {
        render_app_shell(ctx, root);
    } else {
        // Fallback: render children
        for child in &root.children {
            render(ctx, child);
        }
    }
}

/// Render AppShell as windowed layout (egui demo style)
fn render_app_shell_windowed(ctx: &egui::Context, node: &RuntimeNode) {
    let show_bottom = node.get_bool("showBottomPanel").unwrap_or(true);
    
    // Find child components
    let mut activity_bar: Option<&RuntimeNode> = None;
    let mut sidebar: Option<&RuntimeNode> = None;
    let mut main_area: Option<&RuntimeNode> = None;
    let mut bottom_panel: Option<&RuntimeNode> = None;
    let mut status_bar: Option<&RuntimeNode> = None;
    
    for child in &node.children {
        match child.kind.as_str() {
            "ActivityBar" => activity_bar = Some(child),
            "Sidebar" => sidebar = Some(child),
            "MainArea" => main_area = Some(child),
            "BottomPanel" => bottom_panel = Some(child),
            "StatusBar" => status_bar = Some(child),
            _ => {}
        }
    }
    
    // Status bar at bottom
    if let Some(sb) = status_bar {
        egui::TopBottomPanel::bottom("status_bar")
            .exact_height(22.0)
            .frame(egui::Frame::none().fill(colors::STATUSBAR_BG))
            .show(ctx, |ui| {
                render_status_bar(ui, sb);
            });
    }
    
    // Main background
    egui::CentralPanel::default()
        .frame(egui::Frame::none().fill(Color32::from_rgb(220, 220, 220)))
        .show(ctx, |_ui| {});
    
    // Explorer window (left side)
    if let Some(ab) = activity_bar {
        if let Some(sb) = sidebar {
            let active_panel = ab.get_string("active").unwrap_or("files");
            let title = match active_panel {
                "files" => "📁 Explorer",
                "search" => "🔍 Search", 
                "settings" => "⚙ Settings",
                _ => "Explorer",
            };
            
            egui::Window::new(title)
                .default_pos([10.0, 10.0])
                .default_size([220.0, 400.0])
                .resizable(true)
                .collapsible(true)
                .show(ctx, |ui| {
                    // Activity bar as horizontal tabs
                    ui.horizontal(|ui| {
                        render_activity_tabs(ui, ab);
                    });
                    ui.separator();
                    
                    // Sidebar content
                    ScrollArea::vertical().show(ui, |ui| {
                        for child in &sb.children {
                            render_semantic_node(ui, child);
                        }
                    });
                });
        }
    }
    
    // Editor window
    if let Some(ma) = main_area {
        egui::Window::new("📄 Editor")
            .default_pos([250.0, 10.0])
            .default_size([600.0, 400.0])
            .resizable(true)
            .collapsible(true)
            .show(ctx, |ui| {
                for child in &ma.children {
                    match child.kind.as_str() {
                        "TabBar" => {
                            let active = child.get_string("active").unwrap_or("");
                            if !active.is_empty() {
                                let label = active.rsplit('/').next().unwrap_or(&active);
                                ui.horizontal(|ui| {
                                    ui.selectable_label(true, format!("📄 {}", label));
                                });
                                ui.separator();
                            }
                        }
                        "CodeEditor" => render_code_editor_compact(ui, child),
                        _ => render_semantic_node(ui, child),
                    }
                }
            });
    }
    
    // Bottom panel window
    if show_bottom {
        if let Some(bp) = bottom_panel {
            egui::Window::new("🖥 Terminal")
                .default_pos([250.0, 430.0])
                .default_size([600.0, 180.0])
                .resizable(true)
                .collapsible(true)
                .show(ctx, |ui| {
                    // Tab bar
                    for child in &bp.children {
                        match child.kind.as_str() {
                            "TabBar" => render_bottom_tabs(ui, child),
                            "Terminal" => {
                                ui.separator();
                                render_terminal(ui, child);
                            }
                            _ => {}
                        }
                    }
                });
        }
    }
}

fn render_activity_tabs(ui: &mut egui::Ui, node: &RuntimeNode) {
    let active = node.get_string("active").unwrap_or("");
    let items = vec![("files", "📁"), ("search", "🔍"), ("settings", "⚙")];
    
    for (id, icon) in items {
        let is_active = id == active;
        if ui.selectable_label(is_active, icon).clicked() && !is_active {
            if let Some(action) = node.events.get("onSelect") {
                let mut a = action.clone();
                a.args.insert("id".to_string(), Value::String(id.to_string()));
                PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
            }
        }
    }
}

fn render_bottom_tabs(ui: &mut egui::Ui, node: &RuntimeNode) {
    let active = node.get_string("active").unwrap_or("");
    let tabs = vec![("terminal", "Terminal"), ("output", "Output"), ("problems", "Problems")];
    
    ui.horizontal(|ui| {
        for (id, label) in tabs {
            let is_active = id == active;
            if ui.selectable_label(is_active, label).clicked() && !is_active {
                if let Some(action) = node.events.get("onSelect") {
                    let mut a = action.clone();
                    a.args.insert("id".to_string(), Value::String(id.to_string()));
                    PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                }
            }
        }
        
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if node.events.contains_key("onClose") {
                if ui.small_button("✕").clicked() {
                    if let Some(action) = node.events.get("onClose") {
                        PENDING_ACTIONS.with(|c| c.borrow_mut().push(action.clone()));
                    }
                }
            }
        });
    });
}

fn render_code_editor_compact(ui: &mut egui::Ui, node: &RuntimeNode) {
    let content = node.get_string("content").unwrap_or("").to_string();
    let editor_id = "main_editor".to_string();
    
    EDITOR_BUFFERS.with(|cell| {
        let mut buffers = cell.borrow_mut();
        let buffer = buffers.entry(editor_id.clone()).or_insert_with(|| content.clone());
        
        if *buffer != content {
            *buffer = content.clone();
        }
        
        ScrollArea::both().show(ui, |ui| {
            let mut text = buffer.clone();
            let edit = TextEdit::multiline(&mut text)
                .font(FontId::monospace(13.0))
                .code_editor()
                .desired_width(f32::INFINITY);
            
            if ui.add(edit).changed() {
                *buffer = text.clone();
                if let Some(action) = node.events.get("onChange") {
                    let mut a = action.clone();
                    a.args.insert("content".to_string(), Value::String(text));
                    PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                }
            }
        });
    });
}

fn render_app_shell(ctx: &egui::Context, node: &RuntimeNode) {
    let show_bottom = node.get_bool("showBottomPanel").unwrap_or(true);
    
    // Find child components
    let mut activity_bar: Option<&RuntimeNode> = None;
    let mut sidebar: Option<&RuntimeNode> = None;
    let mut main_area: Option<&RuntimeNode> = None;
    let mut bottom_panel: Option<&RuntimeNode> = None;
    let mut status_bar: Option<&RuntimeNode> = None;
    
    for child in &node.children {
        match child.kind.as_str() {
            "ActivityBar" => activity_bar = Some(child),
            "Sidebar" => sidebar = Some(child),
            "MainArea" => main_area = Some(child),
            "BottomPanel" => bottom_panel = Some(child),
            "StatusBar" => status_bar = Some(child),
            _ => {}
        }
    }
    
    // Status bar at bottom
    if let Some(sb) = status_bar {
        egui::TopBottomPanel::bottom("status_bar")
            .exact_height(22.0)
            .frame(egui::Frame::none().fill(colors::STATUSBAR_BG))
            .show(ctx, |ui| {
                render_status_bar(ui, sb);
            });
    }
    
    // Bottom panel
    if show_bottom {
        if let Some(bp) = bottom_panel {
            let height = bp.get_float("height").unwrap_or(180.0) as f32;
            egui::TopBottomPanel::bottom("bottom_panel")
                .resizable(true)
                .min_height(80.0)
                .default_height(height)
                .show(ctx, |ui| {
                    render_bottom_panel_styled(ui, bp);
                });
        }
    }
    
    // Sidebar with activity bar integrated
    if let Some(sb) = sidebar {
        let width = sb.get_float("width").unwrap_or(220.0) as f32;
        egui::SidePanel::left("sidebar")
            .resizable(true)
            .default_width(width)
            .min_width(150.0)
            .show(ctx, |ui| {
                // Activity tabs at top
                if let Some(ab) = activity_bar {
                    render_activity_tabs_styled(ui, ab);
                    ui.separator();
                }
                
                // Sidebar content with collapsing headers
                ScrollArea::vertical().show(ui, |ui| {
                    for child in &sb.children {
                        render_semantic_node(ui, child);
                    }
                });
            });
    }
    
    // Main area
    egui::CentralPanel::default().show(ctx, |ui| {
        if let Some(ma) = main_area {
            render_main_area_styled(ui, ma);
        }
    });
}

fn render_activity_tabs_styled(ui: &mut egui::Ui, node: &RuntimeNode) {
    let active = node.get_string("active").unwrap_or("");
    let items = vec![("files", "📁 Explorer"), ("search", "🔍 Search"), ("settings", "⚙ Settings")];
    
    ui.horizontal(|ui| {
        for (id, label) in items {
            let is_active = id == active;
            if ui.selectable_label(is_active, label).clicked() && !is_active {
                if let Some(action) = node.events.get("onSelect") {
                    let mut a = action.clone();
                    a.args.insert("id".to_string(), Value::String(id.to_string()));
                    PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                }
            }
        }
    });
}

fn render_main_area_styled(ui: &mut egui::Ui, node: &RuntimeNode) {
    for child in &node.children {
        match child.kind.as_str() {
            "TabBar" => {
                let active = child.get_string("active").unwrap_or("");
                if !active.is_empty() {
                    let label = active.rsplit('/').next().unwrap_or(&active);
                    ui.horizontal(|ui| {
                        ui.selectable_label(true, format!("📄 {}", label));
                    });
                    ui.separator();
                }
            }
            "CodeEditor" => {
                let content = child.get_string("content").unwrap_or("").to_string();
                let editor_id = "main_editor".to_string();
                
                EDITOR_BUFFERS.with(|cell| {
                    let mut buffers = cell.borrow_mut();
                    let buffer = buffers.entry(editor_id.clone()).or_insert_with(|| content.clone());
                    
                    if *buffer != content {
                        *buffer = content.clone();
                    }
                    
                    ScrollArea::both().show(ui, |ui| {
                        let mut text = buffer.clone();
                        let edit = TextEdit::multiline(&mut text)
                            .font(FontId::monospace(13.0))
                            .code_editor()
                            .desired_width(f32::INFINITY);
                        
                        if ui.add(edit).changed() {
                            *buffer = text.clone();
                            if let Some(action) = child.events.get("onChange") {
                                let mut a = action.clone();
                                a.args.insert("content".to_string(), Value::String(text));
                                PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                            }
                        }
                    });
                });
            }
            _ => render_semantic_node(ui, child),
        }
    }
}

fn render_bottom_panel_styled(ui: &mut egui::Ui, node: &RuntimeNode) {
    for child in &node.children {
        match child.kind.as_str() {
            "TabBar" => {
                let active = child.get_string("active").unwrap_or("");
                let tabs = vec![("terminal", "Terminal"), ("output", "Output"), ("problems", "Problems")];
                
                ui.horizontal(|ui| {
                    for (id, label) in tabs {
                        let is_active = id == active;
                        if ui.selectable_label(is_active, label).clicked() && !is_active {
                            if let Some(action) = child.events.get("onSelect") {
                                let mut a = action.clone();
                                a.args.insert("id".to_string(), Value::String(id.to_string()));
                                PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                            }
                        }
                    }
                    
                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        if child.events.contains_key("onClose") {
                            if ui.small_button("✕").clicked() {
                                if let Some(action) = child.events.get("onClose") {
                                    PENDING_ACTIONS.with(|c| c.borrow_mut().push(action.clone()));
                                }
                            }
                        }
                    });
                });
                ui.separator();
            }
            "Terminal" => {
                let lines = vec![
                    ("$ ./d.py vibe", false),
                    ("Building vo-cli...", false),
                    ("Running Vibe Studio...", false),
                    ("[VO:OK]", true),
                ];
                
                ScrollArea::vertical().stick_to_bottom(true).show(ui, |ui| {
                    for (text, is_success) in &lines {
                        let color = if *is_success { Color32::from_rgb(76, 175, 80) } else { colors::TEXT_PRIMARY };
                        ui.label(RichText::new(*text).family(egui::FontFamily::Monospace).size(12.0).color(color));
                    }
                });
            }
            _ => {}
        }
    }
}

fn render_activity_bar(ui: &mut egui::Ui, node: &RuntimeNode) {
    ui.vertical_centered(|ui| {
        ui.add_space(12.0);
        
        let active = node.get_string("active").unwrap_or("");
        
        let items = vec![
            ("files", "📁", "Explorer"),
            ("search", "🔍", "Search"),
            ("settings", "⚙", "Settings"),
        ];
        
        for (id, icon, tooltip) in items {
            let is_active = id == active;
            
            let btn = egui::Button::new(RichText::new(icon).size(22.0))
                .min_size(egui::vec2(42.0, 42.0))
                .fill(if is_active { colors::LIST_SELECTED } else { Color32::TRANSPARENT })
                .rounding(6.0);
            
            let response = ui.add(btn).on_hover_text(tooltip);
            
            if response.clicked() {
                if let Some(action) = node.events.get("onSelect") {
                    let mut a = action.clone();
                    a.args.insert("id".to_string(), Value::String(id.to_string()));
                    PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                }
            }
            
            ui.add_space(4.0);
        }
    });
}

fn render_sidebar(ui: &mut egui::Ui, node: &RuntimeNode) {
    ScrollArea::vertical().show(ui, |ui| {
        ui.set_min_width(ui.available_width());
        for child in &node.children {
            render_semantic_node(ui, child);
        }
    });
}

fn render_main_area(ui: &mut egui::Ui, node: &RuntimeNode) {
    ui.vertical(|ui| {
        for child in &node.children {
            match child.kind.as_str() {
                "TabBar" => render_tab_bar(ui, child),
                "CodeEditor" => render_code_editor(ui, child),
                _ => render_semantic_node(ui, child),
            }
        }
    });
}

fn render_bottom_panel(ui: &mut egui::Ui, node: &RuntimeNode) {
    ui.vertical(|ui| {
        for child in &node.children {
            match child.kind.as_str() {
                "TabBar" => render_tab_bar(ui, child),
                "Terminal" => render_terminal(ui, child),
                "ListView" => render_list_view(ui, child),
                _ => render_semantic_node(ui, child),
            }
        }
    });
}

fn render_tab_bar(ui: &mut egui::Ui, node: &RuntimeNode) {
    let active = node.get_string("active").unwrap_or("");
    let has_close = node.events.contains_key("onClose");
    let has_select = node.events.contains_key("onSelect");
    
    ui.horizontal(|ui| {
        ui.style_mut().spacing.item_spacing = egui::vec2(0.0, 0.0);
        
        // For main editor tab bar, show current file
        if !has_select && !has_close {
            if !active.is_empty() {
                let label = active.rsplit('/').next().unwrap_or(&active);
                ui.add_space(12.0);
                
                // Tab with icon
                ui.horizontal(|ui| {
                    ui.label(RichText::new("📄").size(12.0));
                    ui.add_space(4.0);
                    ui.label(RichText::new(label).size(12.0));
                    ui.add_space(8.0);
                    if ui.small_button("✕").clicked() {
                        // Close tab (not implemented)
                    }
                });
            }
        } else {
            // Bottom panel tabs
            ui.add_space(8.0);
            let tabs = vec![
                ("terminal", "TERMINAL"),
                ("output", "OUTPUT"),
                ("problems", "PROBLEMS"),
            ];
            
            for (id, label) in tabs {
                let is_active = id == active;
                let text = RichText::new(label).size(11.0).color(
                    if is_active { colors::TEXT_PRIMARY } else { colors::TEXT_SECONDARY }
                );
                
                let response = ui.selectable_label(is_active, text);
                ui.add_space(12.0);
                
                if response.clicked() && !is_active {
                    if let Some(action) = node.events.get("onSelect") {
                        let mut a = action.clone();
                        a.args.insert("id".to_string(), Value::String(id.to_string()));
                        PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                    }
                }
            }
        }
        
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            if has_close {
                ui.add_space(8.0);
                if ui.small_button("✕").clicked() {
                    if let Some(action) = node.events.get("onClose") {
                        PENDING_ACTIONS.with(|c| c.borrow_mut().push(action.clone()));
                    }
                }
            }
        });
    });
}

fn render_code_editor(ui: &mut egui::Ui, node: &RuntimeNode) {
    let content = node.get_string("content").unwrap_or("").to_string();
    let _language = node.get_string("language").unwrap_or("plain");
    
    let editor_id = "main_editor".to_string();
    
    EDITOR_BUFFERS.with(|cell| {
        let mut buffers = cell.borrow_mut();
        let buffer = buffers.entry(editor_id.clone()).or_insert_with(|| content.clone());
        
        // Sync external changes
        if *buffer != content {
            *buffer = content.clone();
        }
        
        let available = ui.available_size();
        ScrollArea::both()
            .auto_shrink([false, false])
            .show(ui, |ui| {
                let mut text = buffer.clone();
                let edit = TextEdit::multiline(&mut text)
                    .font(FontId::monospace(14.0))
                    .code_editor()
                    .desired_width(available.x)
                    .min_size(available);
                
                if ui.add(edit).changed() {
                    *buffer = text.clone();
                    if let Some(action) = node.events.get("onChange") {
                        let mut a = action.clone();
                        a.args.insert("content".to_string(), Value::String(text));
                        PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                    }
                }
            });
    });
}

fn render_terminal(ui: &mut egui::Ui, node: &RuntimeNode) {
    let output_json = node.get_string("output").unwrap_or("[]");
    let lines = parse_terminal_output(output_json);
    
    ScrollArea::vertical()
        .auto_shrink([false, false])
        .stick_to_bottom(true)
        .show(ui, |ui| {
            ui.set_min_width(ui.available_width());
            ui.add_space(4.0);
            
            if lines.is_empty() {
                ui.label(RichText::new("Terminal ready.").family(egui::FontFamily::Monospace).size(12.0).color(colors::TEXT_SECONDARY));
            } else {
                for (text, style) in &lines {
                    let color = match style.as_str() {
                        "error" => Color32::from_rgb(244, 67, 54),
                        "warning" => Color32::from_rgb(255, 152, 0),
                        "success" => Color32::from_rgb(76, 175, 80),
                        "info" => Color32::from_rgb(33, 150, 243),
                        _ => colors::TEXT_PRIMARY,
                    };
                    
                    ui.label(RichText::new(text).family(egui::FontFamily::Monospace).size(12.0).color(color));
                }
            }
        });
}

fn parse_terminal_output(json: &str) -> Vec<(String, String)> {
    let mut lines = Vec::new();
    
    if json.len() <= 2 {
        return lines;
    }
    
    let content = &json[1..json.len()-1];
    if content.is_empty() {
        return lines;
    }
    
    let mut depth = 0;
    let mut start = 0;
    let chars: Vec<char> = content.chars().collect();
    
    for (i, c) in chars.iter().enumerate() {
        match c {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    let obj: String = chars[start..=i].iter().collect();
                    if let Some((text, style)) = parse_line_object(&obj) {
                        lines.push((text, style));
                    }
                    start = i + 2;
                }
            }
            _ => {}
        }
    }
    
    lines
}

fn parse_line_object(obj: &str) -> Option<(String, String)> {
    let mut text = String::new();
    let mut style = String::from("normal");
    
    if let Some(text_start) = obj.find("\"text\":\"") {
        let start = text_start + 8;
        if let Some(end) = find_string_end(obj, start) {
            text = unescape_json(&obj[start..end]);
        }
    }
    
    if let Some(style_start) = obj.find("\"style\":\"") {
        let start = style_start + 9;
        if let Some(end) = find_string_end(obj, start) {
            style = obj[start..end].to_string();
        }
    }
    
    if text.is_empty() {
        None
    } else {
        Some((text, style))
    }
}

fn find_string_end(s: &str, start: usize) -> Option<usize> {
    let chars: Vec<char> = s.chars().collect();
    let mut i = start;
    while i < chars.len() {
        if chars[i] == '"' && (i == 0 || chars[i-1] != '\\') {
            return Some(i);
        }
        i += 1;
    }
    None
}

fn unescape_json(s: &str) -> String {
    s.replace("\\n", "\n")
     .replace("\\r", "\r")
     .replace("\\t", "\t")
     .replace("\\\"", "\"")
     .replace("\\\\", "\\")
}

fn render_list_view(ui: &mut egui::Ui, node: &RuntimeNode) {
    let items = get_array_prop(node, "items");
    
    ScrollArea::vertical().show(ui, |ui| {
        ui.set_min_width(ui.available_width());
        
        for item in items {
            let primary = get_struct_field(&item, "primary").unwrap_or_default();
            ui.horizontal(|ui| {
                ui.add_space(8.0);
                ui.label(RichText::new(&primary).size(12.0));
            });
        }
    });
}

fn render_file_tree(ui: &mut egui::Ui, node: &RuntimeNode) {
    let selected = node.get_string("selected").unwrap_or("");
    
    // Use egui's native CollapsingHeader for tree view
    egui::CollapsingHeader::new("📁 vibe-studio")
        .default_open(true)
        .show(ui, |ui| {
            // Files
            if ui.selectable_label(selected == "vibe-studio/main.vo", "   📄 main.vo").clicked() {
                dispatch_file_select(node, "vibe-studio/main.vo");
            }
            if ui.selectable_label(selected == "vibe-studio/print_tree.vo", "   📄 print_tree.vo").clicked() {
                dispatch_file_select(node, "vibe-studio/print_tree.vo");
            }
            
            // system folder
            egui::CollapsingHeader::new("📁 system")
                .default_open(false)
                .show(ui, |ui| {
                    if ui.selectable_label(selected == "vibe-studio/system/runtime.vo", "   📄 runtime.vo").clicked() {
                        dispatch_file_select(node, "vibe-studio/system/runtime.vo");
                    }
                });
            
            // ui folder
            egui::CollapsingHeader::new("📁 ui")
                .default_open(false)
                .show(ui, |ui| {
                    if ui.selectable_label(selected == "vibe-studio/ui/app.detra", "   📄 app.detra").clicked() {
                        dispatch_file_select(node, "vibe-studio/ui/app.detra");
                    }
                });
        });
}

fn dispatch_file_select(node: &RuntimeNode, path: &str) {
    if let Some(action) = node.events.get("onSelect") {
        let mut a = action.clone();
        a.args.insert("path".to_string(), Value::String(path.to_string()));
        PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
    }
}

fn render_file_item(ui: &mut egui::Ui, path: &str, name: &str, selected: &str, tree_node: &RuntimeNode, depth: usize) {
    let indent = depth as f32 * 16.0;
    let is_selected = path == selected;
    let label = format!("📄 {}", name);
    
    ui.horizontal(|ui| {
        ui.add_space(indent + 8.0);
        let response = ui.selectable_label(is_selected, RichText::new(&label).size(12.0));
        if response.clicked() {
            if let Some(action) = tree_node.events.get("onSelect") {
                let mut a = action.clone();
                a.args.insert("path".to_string(), Value::String(path.to_string()));
                PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
            }
        }
    });
}

fn render_file_tree_item(ui: &mut egui::Ui, item: &Value, selected: &str, tree_node: &RuntimeNode, depth: usize) {
    let path = get_struct_field(item, "path").unwrap_or_default();
    let name = get_struct_field(item, "name").unwrap_or_default();
    let is_dir = get_struct_bool(item, "isDir");
    let children = get_struct_array(item, "children");
    
    let is_selected = path == selected;
    let indent = depth as f32 * 16.0;
    
    if is_dir {
        let expanded_key = path.clone();
        let expanded = TREE_EXPANDED.with(|cell| {
            *cell.borrow_mut().entry(expanded_key.clone()).or_insert(true)
        });
        
        let icon = if expanded { "▼" } else { "▶" };
        let header = format!("{} 📁 {}", icon, name);
        
        ui.horizontal(|ui| {
            ui.add_space(indent + 4.0);
            let response = ui.selectable_label(false, RichText::new(&header).size(12.0));
            if response.clicked() {
                TREE_EXPANDED.with(|cell| {
                    let mut map = cell.borrow_mut();
                    let v = map.entry(expanded_key).or_insert(true);
                    *v = !*v;
                });
            }
        });
        
        if expanded {
            for child in children {
                render_file_tree_item(ui, &child, selected, tree_node, depth + 1);
            }
        }
    } else {
        let icon = "📄";
        let label = format!("{} {}", icon, name);
        
        ui.horizontal(|ui| {
            ui.add_space(indent + 4.0);
            let response = ui.selectable_label(is_selected, RichText::new(&label).size(12.0));
            if response.clicked() {
                if let Some(action) = tree_node.events.get("onSelect") {
                    let mut a = action.clone();
                    a.args.insert("path".to_string(), Value::String(path));
                    PENDING_ACTIONS.with(|c| c.borrow_mut().push(a));
                }
            }
        });
    }
}

fn render_search_panel(ui: &mut egui::Ui, _node: &RuntimeNode) {
    ui.vertical(|ui| {
        ui.add_space(8.0);
        ui.horizontal(|ui| {
            ui.add_space(8.0);
            ui.label(RichText::new("SEARCH").size(11.0).strong().color(colors::TEXT_SECONDARY));
        });
        ui.add_space(4.0);
        ui.horizontal(|ui| {
            ui.add_space(8.0);
            let mut query = String::new();
            ui.add(TextEdit::singleline(&mut query).hint_text("Search...").desired_width(ui.available_width() - 16.0));
        });
    });
}

fn render_status_bar(ui: &mut egui::Ui, node: &RuntimeNode) {
    ui.horizontal(|ui| {
        ui.add_space(12.0);
        
        let mut left_items = Vec::new();
        let mut right_items = Vec::new();
        
        for child in &node.children {
            if child.kind == "StatusBarItem" {
                let align = child.get_string("align").unwrap_or("left");
                if align == "right" {
                    right_items.push(child);
                } else {
                    left_items.push(child);
                }
            }
        }
        
        for item in left_items {
            let text = item.get_string("text").unwrap_or("");
            ui.label(RichText::new(text).size(11.0).color(Color32::WHITE));
            ui.add_space(20.0);
        }
        
        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            ui.add_space(12.0);
            for item in right_items.iter().rev() {
                let text = item.get_string("text").unwrap_or("");
                ui.label(RichText::new(text).size(11.0).color(Color32::WHITE));
                ui.add_space(20.0);
            }
        });
    });
}

fn render_semantic_node(ui: &mut egui::Ui, node: &RuntimeNode) {
    match node.kind.as_str() {
        "FileTree" => render_file_tree(ui, node),
        "SearchPanel" => render_search_panel(ui, node),
        "Terminal" => render_terminal(ui, node),
        "ListView" => render_list_view(ui, node),
        "CodeEditor" => render_code_editor(ui, node),
        "Column" => {
            ui.vertical(|ui| {
                let padding = node.get_float("padding").unwrap_or(0.0) as f32;
                if padding > 0.0 {
                    ui.add_space(padding);
                }
                for child in &node.children {
                    render_semantic_node(ui, child);
                }
            });
        }
        "Text" => {
            let text = node.get_string("text").unwrap_or("");
            let size = node.get_float("size").unwrap_or(14.0) as f32;
            let bold = node.get_bool("bold").unwrap_or(false);
            
            let mut rt = RichText::new(text).size(size);
            if bold {
                rt = rt.strong();
            }
            ui.label(rt);
        }
        _ => {
            // Render children for unknown nodes
            for child in &node.children {
                render_semantic_node(ui, child);
            }
        }
    }
}

// Helper functions for extracting values from props

fn get_array_prop(node: &RuntimeNode, key: &str) -> Vec<Value> {
    match node.props.get(key) {
        Some(Value::Array(arr)) => arr.clone(),
        _ => Vec::new(),
    }
}

fn get_struct_field(value: &Value, field: &str) -> Option<String> {
    match value {
        Value::Struct(_, fields) | Value::Map(fields) => {
            fields.get(field).and_then(|v| match v {
                Value::String(s) => Some(s.clone()),
                _ => None,
            })
        }
        _ => None,
    }
}

fn get_struct_bool(value: &Value, field: &str) -> bool {
    match value {
        Value::Struct(_, fields) | Value::Map(fields) => {
            fields.get(field).and_then(|v| match v {
                Value::Bool(b) => Some(*b),
                _ => None,
            }).unwrap_or(false)
        }
        _ => false,
    }
}

fn get_struct_array(value: &Value, field: &str) -> Vec<Value> {
    match value {
        Value::Struct(_, fields) | Value::Map(fields) => {
            fields.get(field).and_then(|v| match v {
                Value::Array(arr) => Some(arr.clone()),
                _ => None,
            }).unwrap_or_default()
        }
        _ => Vec::new(),
    }
}
