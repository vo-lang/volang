//! VS Code theme colors and egui visuals configuration.

use eframe::egui::{Color32, Rounding, Stroke, Visuals, style::WidgetVisuals};

// VS Code Light+ color palette
pub mod colors {
    use super::Color32;
    
    // Backgrounds (egui Light theme style)
    pub const EDITOR_BG: Color32 = Color32::from_rgb(250, 250, 250);    // #fafafa
    pub const SIDEBAR_BG: Color32 = Color32::from_rgb(240, 240, 240);   // #f0f0f0
    pub const ACTIVITY_BAR_BG: Color32 = Color32::from_rgb(230, 230, 230); // #e6e6e6
    pub const TITLEBAR_BG: Color32 = Color32::from_rgb(240, 240, 240);  // #f0f0f0
    pub const MENUBAR_BG: Color32 = Color32::from_rgb(240, 240, 240);   // #f0f0f0
    pub const TABBAR_BG: Color32 = Color32::from_rgb(235, 235, 235);    // #ebebeb
    pub const STATUSBAR_BG: Color32 = Color32::from_rgb(0, 122, 204);   // #007acc
    
    // Borders
    pub const BORDER: Color32 = Color32::from_rgb(64, 64, 64); // #404040
    pub const BORDER_LIGHT: Color32 = Color32::from_rgb(86, 86, 86); // #565656
    pub const FOCUS_BORDER: Color32 = Color32::from_rgb(79, 193, 255); // #4fc1ff
    
    // Text (dark on light background)
    pub const TEXT_PRIMARY: Color32 = Color32::from_rgb(50, 50, 50); // #323232
    pub const TEXT_SECONDARY: Color32 = Color32::from_rgb(100, 100, 100); // #646464
    pub const TEXT_DISABLED: Color32 = Color32::from_rgb(160, 160, 160); // #a0a0a0
    pub const TEXT_LINK: Color32 = Color32::from_rgb(0, 102, 204); // #0066cc
    
    // Buttons (light theme)
    pub const BUTTON_BG: Color32 = Color32::from_rgb(0, 122, 204); // #007acc
    pub const BUTTON_HOVER: Color32 = Color32::from_rgb(0, 102, 178); // #0066b2
    pub const BUTTON_SECONDARY: Color32 = Color32::from_rgb(225, 225, 225); // #e1e1e1
    pub const BUTTON_SECONDARY_HOVER: Color32 = Color32::from_rgb(210, 210, 210); // #d2d2d2
    
    // Input (light theme)
    pub const INPUT_BG: Color32 = Color32::from_rgb(255, 255, 255); // #ffffff
    pub const INPUT_BORDER: Color32 = Color32::from_rgb(200, 200, 200); // #c8c8c8
    pub const INPUT_FOCUS_BORDER: Color32 = Color32::from_rgb(0, 122, 204); // #007acc
    
    // List/Tree (light theme)
    pub const LIST_HOVER: Color32 = Color32::from_rgb(225, 225, 225); // #e1e1e1
    pub const LIST_SELECTED: Color32 = Color32::from_rgb(200, 220, 240); // #c8dcf0
    pub const LIST_ACTIVE: Color32 = Color32::from_rgb(0, 122, 204); // #007acc
    
    // Scrollbar
    pub const SCROLLBAR_BG: Color32 = Color32::from_rgb(37, 37, 38); // #252526
    pub const SCROLLBAR_THUMB: Color32 = Color32::from_rgb(121, 121, 121); // #797979
    
    // Divider
    pub const DIVIDER: Color32 = Color32::from_rgb(64, 64, 64); // #404040
    
    // Syntax highlighting (Dark+)
    pub const SYNTAX_KEYWORD: Color32 = Color32::from_rgb(86, 156, 214); // #569cd6
    pub const SYNTAX_STRING: Color32 = Color32::from_rgb(206, 145, 120); // #ce9178
    pub const SYNTAX_NUMBER: Color32 = Color32::from_rgb(181, 206, 168); // #b5cea8
    pub const SYNTAX_COMMENT: Color32 = Color32::from_rgb(106, 153, 85); // #6a9955
    pub const SYNTAX_FUNCTION: Color32 = Color32::from_rgb(220, 220, 170); // #dcdcaa
    pub const SYNTAX_TYPE: Color32 = Color32::from_rgb(78, 201, 176); // #4ec9b0
    pub const SYNTAX_VARIABLE: Color32 = Color32::from_rgb(156, 220, 254); // #9cdcfe
}

pub fn vscode_visuals() -> Visuals {
    use colors::*;
    
    let mut visuals = Visuals::light();
    
    // Window
    visuals.window_fill = SIDEBAR_BG;
    visuals.window_stroke = Stroke::new(1.0, BORDER);
    visuals.window_rounding = Rounding::same(6.0);
    visuals.window_shadow.color = Color32::from_rgba_unmultiplied(0, 0, 0, 30);
    
    // Panel
    visuals.panel_fill = EDITOR_BG;
    
    // Extreme background (behind everything)
    visuals.extreme_bg_color = INPUT_BG;
    visuals.faint_bg_color = Color32::from_rgb(248, 248, 248);
    
    // Text
    visuals.override_text_color = Some(TEXT_PRIMARY);
    
    // Selection
    visuals.selection.bg_fill = LIST_SELECTED;
    visuals.selection.stroke = Stroke::new(1.0, FOCUS_BORDER);
    
    // Hyperlinks
    visuals.hyperlink_color = TEXT_LINK;
    
    // Widgets
    visuals.widgets.noninteractive = WidgetVisuals {
        bg_fill: Color32::from_rgb(250, 250, 250),
        weak_bg_fill: Color32::from_rgb(245, 245, 245),
        bg_stroke: Stroke::new(1.0, BORDER),
        rounding: Rounding::same(4.0),
        fg_stroke: Stroke::new(1.0, TEXT_PRIMARY),
        expansion: 0.0,
    };
    
    visuals.widgets.inactive = WidgetVisuals {
        bg_fill: BUTTON_SECONDARY,
        weak_bg_fill: Color32::from_rgb(235, 235, 235),
        bg_stroke: Stroke::new(1.0, BORDER),
        rounding: Rounding::same(4.0),
        fg_stroke: Stroke::new(1.0, TEXT_PRIMARY),
        expansion: 0.0,
    };
    
    visuals.widgets.hovered = WidgetVisuals {
        bg_fill: LIST_HOVER,
        weak_bg_fill: Color32::from_rgb(220, 220, 220),
        bg_stroke: Stroke::NONE,
        rounding: Rounding::same(4.0),
        fg_stroke: Stroke::new(1.0, TEXT_PRIMARY),
        expansion: 0.0,
    };
    
    visuals.widgets.active = WidgetVisuals {
        bg_fill: BUTTON_BG,
        weak_bg_fill: BUTTON_BG,
        bg_stroke: Stroke::new(1.0, FOCUS_BORDER),
        rounding: Rounding::same(4.0),
        fg_stroke: Stroke::new(2.0, Color32::WHITE),
        expansion: 1.0,
    };
    
    visuals.widgets.open = WidgetVisuals {
        bg_fill: Color32::from_rgb(240, 240, 240),
        weak_bg_fill: Color32::from_rgb(245, 245, 245),
        bg_stroke: Stroke::new(1.0, FOCUS_BORDER),
        rounding: Rounding::same(4.0),
        fg_stroke: Stroke::new(1.0, TEXT_PRIMARY),
        expansion: 0.0,
    };
    
    visuals
}
