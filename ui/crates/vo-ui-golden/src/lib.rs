#![no_std]

extern crate alloc;

use alloc::string::String;
use core::fmt::{self, Write};
use vo_ui_accessibility::{
    AccessibilityActions, AccessibilityRole, AccessibilityState, AccessibilityTree,
};
use vo_ui_core::NodeId;
use vo_ui_layout::Rect;
use vo_ui_paint::{DrawCommand, PaintScene};

pub const ACCESSIBILITY_SNAPSHOT_VERSION: &str = "VAX1";
pub const PAINT_SNAPSHOT_VERSION: &str = "VPX2";

/// Produces the renderer-neutral semantic golden shared by browser and desktop
/// conformance tests. Geometry stays in the paint golden because platform
/// accessibility APIs can quantize bounds differently.
pub fn accessibility_snapshot(tree: &AccessibilityTree) -> String {
    let mut output = String::new();
    writeln!(
        output,
        "{ACCESSIBILITY_SNAPSHOT_VERSION} revision={} root={}",
        tree.revision, tree.root
    )
    .expect("writing to a String cannot fail");
    for node in tree.iter() {
        write!(output, "node={} role=", node.id).expect("writing to a String cannot fail");
        write_quoted(&mut output, role_name(&node.role));
        write!(output, " name=").expect("writing to a String cannot fail");
        write_quoted(&mut output, &node.name);
        write!(output, " description=").expect("writing to a String cannot fail");
        write_quoted(&mut output, &node.description);
        write!(output, " value=").expect("writing to a String cannot fail");
        write_quoted(&mut output, &node.value);
        write!(
            output,
            " state={} actions={} children=",
            state_bits(node.state),
            action_bits(node.actions)
        )
        .expect("writing to a String cannot fail");
        write_node_list(&mut output, &node.children);
        output.push('\n');
    }
    output
}

/// Produces a deterministic logical paint golden. Values are rounded to one
/// thousandth of a logical pixel so harmless backend floating-point noise does
/// not invalidate a cross-platform baseline.
pub fn paint_snapshot(scene: &PaintScene) -> String {
    let mut output = String::new();
    writeln!(
        output,
        "{PAINT_SNAPSHOT_VERSION} revision={} viewport={:.3},{:.3}",
        scene.revision,
        stable(scene.viewport.width),
        stable(scene.viewport.height),
    )
    .expect("writing to a String cannot fail");
    for command in scene.commands() {
        match command {
            DrawCommand::FillRect {
                node,
                rect,
                clip,
                color,
                radius,
            } => {
                write!(output, "fill node={node} rect=").expect("writing to a String cannot fail");
                write_rect(&mut output, *rect);
                write!(output, " clip=").expect("writing to a String cannot fail");
                write_optional_rect(&mut output, *clip);
                writeln!(output, " color={color:08x} radius={:.3}", stable(*radius))
                    .expect("writing to a String cannot fail");
            }
            DrawCommand::StrokeRect {
                node,
                rect,
                clip,
                color,
                radius,
                width,
            } => {
                write!(output, "stroke node={node} rect=")
                    .expect("writing to a String cannot fail");
                write_rect(&mut output, *rect);
                write!(output, " clip=").expect("writing to a String cannot fail");
                write_optional_rect(&mut output, *clip);
                writeln!(
                    output,
                    " color={color:08x} radius={:.3} width={:.3}",
                    stable(*radius),
                    stable(*width)
                )
                .expect("writing to a String cannot fail");
            }
            DrawCommand::Text {
                node,
                rect,
                clip,
                color,
                font_size,
                font_weight,
                value,
            } => {
                write!(output, "text node={node} rect=").expect("writing to a String cannot fail");
                write_rect(&mut output, *rect);
                write!(output, " clip=").expect("writing to a String cannot fail");
                write_optional_rect(&mut output, *clip);
                write!(
                    output,
                    " color={color:08x} size={:.3} weight={font_weight} value=",
                    stable(*font_size)
                )
                .expect("writing to a String cannot fail");
                write_quoted(&mut output, value);
                output.push('\n');
            }
            DrawCommand::TextEditor {
                node,
                rect,
                clip,
                color,
                font_size,
                font_weight,
                value,
                placeholder,
                selection_start_utf16,
                selection_length_utf16,
            } => {
                write!(output, "editor node={node} rect=")
                    .expect("writing to a String cannot fail");
                write_rect(&mut output, *rect);
                write!(output, " clip=").expect("writing to a String cannot fail");
                write_optional_rect(&mut output, *clip);
                write!(
                    output,
                    " color={color:08x} size={:.3} weight={font_weight} selection={selection_start_utf16},{selection_length_utf16} value=",
                    stable(*font_size)
                )
                .expect("writing to a String cannot fail");
                write_quoted(&mut output, value);
                write!(output, " placeholder=").expect("writing to a String cannot fail");
                write_quoted(&mut output, placeholder);
                output.push('\n');
            }
            DrawCommand::Scrollbar {
                node,
                track,
                thumb,
                color,
            } => {
                write!(output, "scrollbar node={node} track=")
                    .expect("writing to a String cannot fail");
                write_rect(&mut output, *track);
                write!(output, " thumb=").expect("writing to a String cannot fail");
                write_rect(&mut output, *thumb);
                writeln!(output, " color={color:08x}").expect("writing to a String cannot fail");
            }
        }
    }
    output
}

fn role_name(role: &AccessibilityRole) -> &str {
    match role {
        AccessibilityRole::Root => "root",
        AccessibilityRole::Group => "group",
        AccessibilityRole::Presentation => "presentation",
        AccessibilityRole::StaticText => "text",
        AccessibilityRole::Paragraph => "paragraph",
        AccessibilityRole::Button => "button",
        AccessibilityRole::TextBox => "textbox",
        AccessibilityRole::Switch => "switch",
        AccessibilityRole::Slider => "slider",
        AccessibilityRole::Image => "image",
        AccessibilityRole::Alert => "alert",
        AccessibilityRole::Dialog => "dialog",
        AccessibilityRole::Heading => "heading",
        AccessibilityRole::Status => "status",
        AccessibilityRole::ProgressIndicator => "progressbar",
        AccessibilityRole::Separator => "separator",
        AccessibilityRole::Link => "link",
        AccessibilityRole::Navigation => "navigation",
        AccessibilityRole::Toolbar => "toolbar",
        AccessibilityRole::List => "list",
        AccessibilityRole::ListItem => "listitem",
        AccessibilityRole::RadioGroup => "radiogroup",
        AccessibilityRole::RadioButton => "radio",
        AccessibilityRole::ComboBox => "combobox",
        AccessibilityRole::ListBox => "listbox",
        AccessibilityRole::Option => "option",
        AccessibilityRole::AlertDialog => "alertdialog",
        AccessibilityRole::Tooltip => "tooltip",
        AccessibilityRole::MenuBar => "menubar",
        AccessibilityRole::Menu => "menu",
        AccessibilityRole::MenuItem => "menuitem",
        AccessibilityRole::MenuItemCheckBox => "menuitemcheckbox",
        AccessibilityRole::MenuItemRadio => "menuitemradio",
        AccessibilityRole::TabList => "tablist",
        AccessibilityRole::Tab => "tab",
        AccessibilityRole::TabPanel => "tabpanel",
        AccessibilityRole::Grid => "grid",
        AccessibilityRole::Row => "row",
        AccessibilityRole::GridCell => "gridcell",
        AccessibilityRole::ColumnHeader => "columnheader",
        AccessibilityRole::RowHeader => "rowheader",
        AccessibilityRole::Tree => "tree",
        AccessibilityRole::TreeItem => "treeitem",
        AccessibilityRole::Custom(value) => value,
    }
}

fn state_bits(state: AccessibilityState) -> SnapshotBits<9> {
    SnapshotBits([
        bit(state.disabled),
        bit(state.required),
        bit(state.invalid),
        bit(state.modal),
        match state.checked {
            None => '-',
            Some(false) => '0',
            Some(true) => '1',
        },
        optional_bit(state.selected),
        optional_bit(state.expanded),
        optional_bit(state.pressed),
        match state.current {
            None => '-',
            Some(vo_ui_accessibility::AccessibilityCurrent::False) => '0',
            Some(vo_ui_accessibility::AccessibilityCurrent::True) => '1',
            Some(vo_ui_accessibility::AccessibilityCurrent::Page) => 'p',
            Some(vo_ui_accessibility::AccessibilityCurrent::Step) => 's',
            Some(vo_ui_accessibility::AccessibilityCurrent::Location) => 'l',
            Some(vo_ui_accessibility::AccessibilityCurrent::Date) => 'd',
            Some(vo_ui_accessibility::AccessibilityCurrent::Time) => 't',
        },
    ])
}

const fn optional_bit(value: Option<bool>) -> char {
    match value {
        None => '-',
        Some(false) => '0',
        Some(true) => '1',
    }
}

fn action_bits(actions: AccessibilityActions) -> SnapshotBits<4> {
    SnapshotBits([
        bit(actions.focus),
        bit(actions.invoke),
        bit(actions.set_value),
        bit(actions.toggle),
    ])
}

const fn bit(value: bool) -> char {
    if value {
        '1'
    } else {
        '0'
    }
}

fn write_node_list(output: &mut String, nodes: &[NodeId]) {
    output.push('[');
    for (index, node) in nodes.iter().enumerate() {
        if index != 0 {
            output.push(',');
        }
        write!(output, "{node}").expect("writing to a String cannot fail");
    }
    output.push(']');
}

fn write_optional_rect(output: &mut String, rect: Option<Rect>) {
    if let Some(rect) = rect {
        write_rect(output, rect);
    } else {
        output.push('-');
    }
}

fn write_rect(output: &mut String, rect: Rect) {
    write!(
        output,
        "{:.3},{:.3},{:.3},{:.3}",
        stable(rect.x),
        stable(rect.y),
        stable(rect.width),
        stable(rect.height)
    )
    .expect("writing to a String cannot fail");
}

fn stable(value: f64) -> f64 {
    if value.abs() < 0.000_5 {
        0.0
    } else {
        value
    }
}

fn write_quoted(output: &mut String, value: &str) {
    output.push('"');
    for character in value.chars() {
        match character {
            '"' => output.push_str("\\\""),
            '\\' => output.push_str("\\\\"),
            '\n' => output.push_str("\\n"),
            '\r' => output.push_str("\\r"),
            '\t' => output.push_str("\\t"),
            value if value.is_control() => {
                write!(output, "\\u{{{:x}}}", value as u32)
                    .expect("writing to a String cannot fail");
            }
            value => output.push(value),
        }
    }
    output.push('"');
}

impl<const N: usize> fmt::Display for SnapshotBits<N> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        for bit in self.0 {
            formatter.write_char(bit)?;
        }
        Ok(())
    }
}

struct SnapshotBits<const N: usize>([char; N]);
