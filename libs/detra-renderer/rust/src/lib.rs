//! Detra Renderer - egui-based rendering engine for Detra UI trees.

use std::cell::RefCell;
use std::collections::HashMap;

use eframe::egui;
use libloading::{Library, Symbol};
use linkme::distributed_slice;
use vo_runtime::ffi::{ExternCallContext, ExternEntryWithContext, ExternResult, EXTERN_TABLE_WITH_CONTEXT};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::string;

use detra_renderable::{RuntimeNode, Value, ActionCall};

// C ABI function type - single function to get tree pointer
type DetraPrepareTree = unsafe extern "C" fn(usize) -> *const RuntimeNode;

lazy_static::lazy_static! {
    static ref DETRA_LIB: Option<Library> = {
        let lib_path = std::env::current_exe()
            .ok()
            .and_then(|p| p.parent().map(|d| d.join("libdetra.so")))
            .unwrap_or_else(|| std::path::PathBuf::from("target/debug/libdetra.so"));
        
        unsafe { Library::new(&lib_path).ok() }
    };
}

thread_local! {
    static CURRENT_TREE: RefCell<Option<RuntimeNode>> = const { RefCell::new(None) };
    static PENDING_ACTIONS: RefCell<Vec<ActionCall>> = const { RefCell::new(Vec::new()) };
    static CURRENT_TREE_ID: RefCell<usize> = const { RefCell::new(0) };
    static CURRENT_ACTION_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    static CURRENT_ACTION_ARGS: RefCell<HashMap<String, Value>> = RefCell::new(HashMap::new());
}

struct DetraApp {
    on_action_closure: GcRef,
    vm: *mut std::ffi::c_void,
    fiber: *mut std::ffi::c_void,
    call_closure_fn: vo_runtime::ffi::ClosureCallFn,
}

impl eframe::App for DetraApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let tree = CURRENT_TREE.with(|cell| cell.borrow().clone());

        egui::CentralPanel::default().show(ctx, |ui| {
            if let Some(tree) = &tree {
                render_node(ui, tree);
            } else {
                ui.label("Loading...");
            }
        });

        let actions: Vec<ActionCall> = PENDING_ACTIONS.with(|cell| {
            std::mem::take(&mut *cell.borrow_mut())
        });

        for action in actions {
            self.dispatch_action(&action);
        }

        ctx.request_repaint();
    }
}

impl DetraApp {
    fn dispatch_action(&self, action: &ActionCall) {
        // Set current action name and args for PendingAction()/PendingActionArg() to read
        CURRENT_ACTION_NAME.with(|cell| {
            *cell.borrow_mut() = action.name.clone();
        });
        CURRENT_ACTION_ARGS.with(|cell| {
            *cell.borrow_mut() = action.args.clone();
        });

        // Call closure with no args
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];

        let _ = (self.call_closure_fn)(
            self.vm,
            self.fiber,
            self.on_action_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );
    }
}

fn render_node(ui: &mut egui::Ui, node: &RuntimeNode) {
    match node.kind.as_str() {
        "Column" => render_column(ui, node),
        "Row" => render_row(ui, node),
        "Text" => render_text(ui, node),
        "Button" => render_button(ui, node),
        "Input" => render_input(ui, node),
        "Checkbox" => render_checkbox(ui, node),
        "Slider" => render_slider(ui, node),
        "Spacer" => render_spacer(ui, node),
        "Divider" => render_divider(ui, node),
        "Card" => render_card(ui, node),
        "Image" => render_image(ui, node),
        "Stack" => render_stack(ui, node),
        "Scroll" => render_scroll(ui, node),
        "Select" => render_select(ui, node),
        "Switch" => render_switch(ui, node),
        "List" => render_list(ui, node),
        "Dialog" => render_dialog(ui, node),
        _ => {
            ui.label(format!("Unknown widget: {}", node.kind));
        }
    }
}

fn render_column(ui: &mut egui::Ui, node: &RuntimeNode) {
    let spacing = get_float_prop(&node.props, "spacing", 4.0);
    let padding = get_float_prop(&node.props, "padding", 0.0);

    egui::Frame::none()
        .inner_margin(padding as f32)
        .show(ui, |ui| {
            ui.spacing_mut().item_spacing.y = spacing as f32;
            ui.vertical(|ui| {
                for child in &node.children {
                    render_node(ui, child);
                }
            });
        });
}

fn render_row(ui: &mut egui::Ui, node: &RuntimeNode) {
    let spacing = get_float_prop(&node.props, "spacing", 4.0);
    let padding = get_float_prop(&node.props, "padding", 0.0);

    egui::Frame::none()
        .inner_margin(padding as f32)
        .show(ui, |ui| {
            ui.spacing_mut().item_spacing.x = spacing as f32;
            ui.horizontal(|ui| {
                for child in &node.children {
                    render_node(ui, child);
                }
            });
        });
}

fn render_text(ui: &mut egui::Ui, node: &RuntimeNode) {
    let text = get_string_prop(&node.props, "text", "");
    let size = get_float_prop(&node.props, "size", 14.0);
    let bold = get_bool_prop(&node.props, "bold", false);

    let text = egui::RichText::new(&text).size(size as f32);
    let text = if bold { text.strong() } else { text };
    ui.label(text);
}

fn render_button(ui: &mut egui::Ui, node: &RuntimeNode) {
    let text = get_string_prop(&node.props, "text", "Button");
    let enabled = get_bool_prop(&node.props, "enabled", true);

    let response = ui.add_enabled(enabled, egui::Button::new(&text));

    if response.clicked() {
        if let Some(action_ref) = node.events.get("onClick") {
            queue_action(action_ref);
        }
    }
}

fn render_input(ui: &mut egui::Ui, node: &RuntimeNode) {
    let value = get_string_prop(&node.props, "value", "");
    let placeholder = get_string_prop(&node.props, "placeholder", "");
    let enabled = get_bool_prop(&node.props, "enabled", true);

    let mut text = value.clone();
    let response = ui.add_enabled(
        enabled,
        egui::TextEdit::singleline(&mut text).hint_text(&placeholder),
    );

    if response.changed() {
        if let Some(action_ref) = node.events.get("onChange") {
            let mut action = ActionCall {
                name: action_ref.name.clone(),
                args: action_ref.args.clone(),
            };
            action.args.insert("value".to_string(), Value::String(text));
            PENDING_ACTIONS.with(|cell| cell.borrow_mut().push(action));
        }
    }
}

fn render_checkbox(ui: &mut egui::Ui, node: &RuntimeNode) {
    let label = get_string_prop(&node.props, "label", "");
    let checked = get_bool_prop(&node.props, "checked", false);
    let enabled = get_bool_prop(&node.props, "enabled", true);

    let mut value = checked;
    let response = ui.add_enabled(enabled, egui::Checkbox::new(&mut value, &label));

    if response.changed() {
        if let Some(action_ref) = node.events.get("onChange") {
            let mut action = ActionCall {
                name: action_ref.name.clone(),
                args: action_ref.args.clone(),
            };
            action.args.insert("checked".to_string(), Value::Bool(value));
            PENDING_ACTIONS.with(|cell| cell.borrow_mut().push(action));
        }
    }
}

fn render_slider(ui: &mut egui::Ui, node: &RuntimeNode) {
    let value = get_float_prop(&node.props, "value", 0.0);
    let min = get_float_prop(&node.props, "min", 0.0);
    let max = get_float_prop(&node.props, "max", 100.0);
    let enabled = get_bool_prop(&node.props, "enabled", true);

    let mut val = value;
    let response = ui.add_enabled(enabled, egui::Slider::new(&mut val, min..=max));

    if response.changed() {
        if let Some(action_ref) = node.events.get("onChange") {
            let mut action = ActionCall {
                name: action_ref.name.clone(),
                args: action_ref.args.clone(),
            };
            action.args.insert("value".to_string(), Value::Float(val));
            PENDING_ACTIONS.with(|cell| cell.borrow_mut().push(action));
        }
    }
}

fn render_spacer(ui: &mut egui::Ui, node: &RuntimeNode) {
    let size = get_float_prop(&node.props, "size", 0.0);
    if size > 0.0 {
        ui.add_space(size as f32);
    } else {
        ui.add_space(ui.available_width().min(ui.available_height()));
    }
}

fn render_divider(ui: &mut egui::Ui, node: &RuntimeNode) {
    let vertical = get_bool_prop(&node.props, "vertical", false);
    if vertical {
        ui.separator();
    } else {
        ui.separator();
    }
}

fn render_card(ui: &mut egui::Ui, node: &RuntimeNode) {
    let title = get_string_prop(&node.props, "title", "");
    let padding = get_float_prop(&node.props, "padding", 8.0);

    egui::Frame::none()
        .stroke(ui.visuals().widgets.noninteractive.bg_stroke)
        .rounding(4.0)
        .inner_margin(padding as f32)
        .show(ui, |ui| {
            if !title.is_empty() {
                ui.heading(&title);
                ui.separator();
            }
            for child in &node.children {
                render_node(ui, child);
            }
        });
}

fn render_image(ui: &mut egui::Ui, node: &RuntimeNode) {
    let _src = get_string_prop(&node.props, "src", "");
    let width = get_float_prop(&node.props, "width", 100.0);
    let height = get_float_prop(&node.props, "height", 100.0);

    ui.allocate_space(egui::vec2(width as f32, height as f32));
}

fn render_stack(ui: &mut egui::Ui, node: &RuntimeNode) {
    for child in &node.children {
        render_node(ui, child);
    }
}

fn render_scroll(ui: &mut egui::Ui, node: &RuntimeNode) {
    let direction = get_string_prop(&node.props, "direction", "vertical");

    let scroll = match direction.as_str() {
        "horizontal" => egui::ScrollArea::horizontal(),
        "both" => egui::ScrollArea::both(),
        _ => egui::ScrollArea::vertical(),
    };

    scroll.show(ui, |ui| {
        for child in &node.children {
            render_node(ui, child);
        }
    });
}

fn render_select(ui: &mut egui::Ui, node: &RuntimeNode) {
    let value = get_string_prop(&node.props, "value", "");
    let options_str = get_string_prop(&node.props, "options", "");
    let enabled = get_bool_prop(&node.props, "enabled", true);

    let options: Vec<&str> = options_str.split(',').map(|s| s.trim()).collect();
    let mut selected = value.clone();

    egui::ComboBox::from_label("")
        .selected_text(&selected)
        .show_ui(ui, |ui| {
            for opt in &options {
                if ui.selectable_value(&mut selected, opt.to_string(), *opt).clicked() {
                    if let Some(action_ref) = node.events.get("onChange") {
                        let mut action = ActionCall {
                            name: action_ref.name.clone(),
                            args: action_ref.args.clone(),
                        };
                        action.args.insert("value".to_string(), Value::String(selected.clone()));
                        PENDING_ACTIONS.with(|cell| cell.borrow_mut().push(action));
                    }
                }
            }
        });

    let _ = enabled;
}

fn render_switch(ui: &mut egui::Ui, node: &RuntimeNode) {
    let on = get_bool_prop(&node.props, "on", false);
    let enabled = get_bool_prop(&node.props, "enabled", true);

    let mut value = on;
    let response = ui.add_enabled(enabled, egui::Checkbox::without_text(&mut value));

    if response.changed() {
        if let Some(action_ref) = node.events.get("onChange") {
            let mut action = ActionCall {
                name: action_ref.name.clone(),
                args: action_ref.args.clone(),
            };
            action.args.insert("checked".to_string(), Value::Bool(value));
            PENDING_ACTIONS.with(|cell| cell.borrow_mut().push(action));
        }
    }
}

fn render_list(ui: &mut egui::Ui, node: &RuntimeNode) {
    egui::ScrollArea::vertical().show(ui, |ui| {
        for child in &node.children {
            render_node(ui, child);
        }
    });
}

fn render_dialog(ui: &mut egui::Ui, node: &RuntimeNode) {
    let title = get_string_prop(&node.props, "title", "Dialog");
    let open = get_bool_prop(&node.props, "open", false);

    if !open {
        return;
    }

    egui::Window::new(&title).show(ui.ctx(), |ui| {
        for child in &node.children {
            render_node(ui, child);
        }
    });
}

fn get_string_prop(props: &HashMap<String, Value>, key: &str, default: &str) -> String {
    match props.get(key) {
        Some(Value::String(s)) => s.clone(),
        _ => default.to_string(),
    }
}

fn get_float_prop(props: &HashMap<String, Value>, key: &str, default: f64) -> f64 {
    match props.get(key) {
        Some(Value::Float(f)) => *f,
        Some(Value::Int(n)) => *n as f64,
        _ => default,
    }
}

fn get_bool_prop(props: &HashMap<String, Value>, key: &str, default: bool) -> bool {
    match props.get(key) {
        Some(Value::Bool(b)) => *b,
        _ => default,
    }
}

fn queue_action(action_ref: &ActionCall) {
    PENDING_ACTIONS.with(|cell| {
        cell.borrow_mut().push(ActionCall {
            name: action_ref.name.clone(),
            args: action_ref.args.clone(),
        });
    });
}

fn detra_renderer_run(ctx: &mut ExternCallContext) -> ExternResult {
    // Config struct layout: Title(string=1slot), Width(int=1), Height(int=1), Resizable(bool=1), VSync(bool=1) = 5 slots
    // Then closure = 1 slot
    // Total args: 6 slots
    let title_ref = ctx.arg_ref(0);  // slot 0: Config.Title
    let width = ctx.arg_i64(1);      // slot 1: Config.Width
    let height = ctx.arg_i64(2);     // slot 2: Config.Height
    let _resizable = ctx.arg_i64(3) != 0;  // slot 3: Config.Resizable
    let _vsync = ctx.arg_i64(4) != 0;      // slot 4: Config.VSync
    let on_action_closure = ctx.arg_ref(5); // slot 5: onAction closure

    let title = string::as_str(title_ref).to_string();

    if !ctx.can_call_closure() {
        return ExternResult::Panic("detra_renderer_run: closure calling not available".to_string());
    }

    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = ctx.closure_call_fn().unwrap();

    let app = DetraApp {
        on_action_closure,
        vm,
        fiber,
        call_closure_fn,
    };

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([width as f32, height as f32])
            .with_title(&title),
        ..Default::default()
    };

    let _ = eframe::run_native(
        &title,
        options,
        Box::new(|_cc| Ok(Box::new(app))),
    );

    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_RENDERER_RUN: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_detra-renderer_Run",
    func: detra_renderer_run,
};

fn detra_renderer_render(ctx: &mut ExternCallContext) -> ExternResult {
    let tree_id = ctx.arg_i64(0) as usize;

    CURRENT_TREE_ID.with(|cell| {
        *cell.borrow_mut() = tree_id;
    });

    // Try to load tree from detra via C ABI
    if let Some(node) = load_tree_from_detra(tree_id) {
        CURRENT_TREE.with(|cell| {
            *cell.borrow_mut() = Some(node);
        });
    }

    ExternResult::Ok
}

fn load_tree_from_detra(tree_id: usize) -> Option<RuntimeNode> {
    let lib = DETRA_LIB.as_ref()?;
    
    unsafe {
        let prepare_tree: Symbol<DetraPrepareTree> = lib.get(b"detra_prepare_tree").ok()?;
        let root = prepare_tree(tree_id);
        if root.is_null() {
            return None;
        }
        Some((*root).clone())
    }
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_RENDERER_RENDER: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_detra-renderer_Render",
    func: detra_renderer_render,
};

fn detra_renderer_pending_action(ctx: &mut ExternCallContext) -> ExternResult {
    let name = CURRENT_ACTION_NAME.with(|cell| cell.borrow().clone());
    ctx.ret_str(0, &name);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_RENDERER_PENDING_ACTION: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_detra-renderer_PendingAction",
    func: detra_renderer_pending_action,
};

fn detra_renderer_pending_action_arg(ctx: &mut ExternCallContext) -> ExternResult {
    let key_ref = ctx.arg_ref(0);
    let key = string::as_str(key_ref);
    
    let value = CURRENT_ACTION_ARGS.with(|cell| {
        let args = cell.borrow();
        match args.get(key) {
            Some(Value::String(s)) => s.clone(),
            _ => String::new(),
        }
    });
    
    ctx.ret_str(0, &value);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_RENDERER_PENDING_ACTION_ARG: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_detra-renderer_PendingActionArg",
    func: detra_renderer_pending_action_arg,
};

pub fn link_detra_renderer_externs() {
    let _ = &__VO_DETRA_RENDERER_RUN;
    let _ = &__VO_DETRA_RENDERER_RENDER;
    let _ = &__VO_DETRA_RENDERER_PENDING_ACTION;
    let _ = &__VO_DETRA_RENDERER_PENDING_ACTION_ARG;
}

vo_ext::export_extensions!();
