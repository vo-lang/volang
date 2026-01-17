//! Detra Renderer - egui-based rendering engine for Detra UI trees.
//!
//! Architecture:
//! - Reads RuntimeNode from detra via CURRENT_TREE
//! - Computes layout using taffy (in this crate)
//! - Renders using egui native widgets for interaction

mod layout;
mod render_tree;
mod semantic;
mod theme;

use std::cell::RefCell;
use std::collections::HashMap;

use eframe::egui::{self, FontData, FontDefinitions, FontFamily};
use linkme::distributed_slice;
use vo_runtime::ffi::{ExternCallContext, ExternEntryWithContext, ExternResult, EXTERN_TABLE_WITH_CONTEXT};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::string;

use detra_renderable::{RuntimeNode, Value, ActionCall};

thread_local! {
    static PENDING_ACTIONS: RefCell<Vec<ActionCall>> = const { RefCell::new(Vec::new()) };
    static CURRENT_ACTION_NAME: RefCell<String> = const { RefCell::new(String::new()) };
    static CURRENT_ACTION_ARGS: RefCell<HashMap<String, Value>> = RefCell::new(HashMap::new());
}

fn configure_fonts(ctx: &egui::Context) {
    let mut fonts = FontDefinitions::default();
    
    // Add Inter Bold font (embedded)
    fonts.font_data.insert(
        "Inter-Bold".to_owned(),
        FontData::from_static(include_bytes!("../assets/Inter-Bold.ttf")),
    );
    
    // Create a Bold family
    fonts.families.insert(
        FontFamily::Name("Bold".into()),
        vec!["Inter-Bold".to_owned(), "Hack".to_owned()],
    );
    
    // Also add Inter Regular for consistency
    fonts.font_data.insert(
        "Inter-Regular".to_owned(),
        FontData::from_static(include_bytes!("../assets/Inter-Regular.ttf")),
    );
    
    // Prepend Inter to default proportional family
    fonts.families.get_mut(&FontFamily::Proportional).unwrap()
        .insert(0, "Inter-Regular".to_owned());
    
    ctx.set_fonts(fonts);
}


fn load_current_tree() -> Option<RuntimeNode> {
    type GetTree = unsafe extern "C" fn() -> *const RuntimeNode;
    #[cfg(unix)]
    let func: Option<GetTree> = unsafe {
        let h = libc::dlsym(libc::RTLD_DEFAULT, b"detra_get_current_tree\0".as_ptr() as *const _);
        if h.is_null() { None } else { Some(std::mem::transmute(h)) }
    };
    #[cfg(not(unix))]
    let func: Option<GetTree> = None;
    
    let ptr = unsafe { func?() };
    if ptr.is_null() { None } else { Some(unsafe { (*ptr).clone() }) }
}

struct DetraApp {
    on_action_closure: GcRef,
    vm: *mut std::ffi::c_void,
    fiber: *mut std::ffi::c_void,
    call_closure_fn: vo_runtime::ffi::ClosureCallFn,
}

impl eframe::App for DetraApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Use egui light theme
        ctx.set_visuals(egui::Visuals::light());
        
        if let Some(tree) = load_current_tree() {
            // Use semantic renderer for semantic components (AppShell, etc.)
            if tree.kind == "AppShell" || tree.children.iter().any(|c| c.kind == "AppShell") {
                semantic::render(ctx, &tree);
            } else {
                // Fallback to layout-based renderer for low-level components
                let screen = ctx.screen_rect();
                let layout_tree = layout::compute_layout(&tree, screen.width(), screen.height());
                egui::CentralPanel::default().show(ctx, |_ui| {
                    render_tree::render(ctx, &layout_tree);
                });
            }
        } else {
            egui::CentralPanel::default().show(ctx, |ui| { ui.label("No tree available"); });
        }

        let actions: Vec<ActionCall> = PENDING_ACTIONS.with(|cell| std::mem::take(&mut *cell.borrow_mut()));
        for action in actions {
            self.dispatch_action(&action);
        }
        ctx.request_repaint();
    }
}

impl DetraApp {
    fn dispatch_action(&self, action: &ActionCall) {
        CURRENT_ACTION_NAME.with(|cell| { *cell.borrow_mut() = action.name.clone(); });
        CURRENT_ACTION_ARGS.with(|cell| { *cell.borrow_mut() = action.args.clone(); });
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _ = (self.call_closure_fn)(self.vm, self.fiber, self.on_action_closure as u64, args.as_ptr(), 0, ret.as_mut_ptr(), 0);
    }
}

fn detra_renderer_run(ctx: &mut ExternCallContext) -> ExternResult {
    let title_ref = ctx.arg_ref(0);
    let title = string::as_str(title_ref).to_string();
    let width = ctx.arg_i64(1) as u32;
    let height = ctx.arg_i64(2) as u32;
    let _resizable = ctx.arg_i64(3) != 0;
    let _vsync = ctx.arg_i64(4) != 0;
    let on_action_closure = ctx.arg_ref(5);
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = ctx.closure_call_fn().unwrap();

    let app = DetraApp { on_action_closure, vm, fiber, call_closure_fn };
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([width as f32, height as f32])
            .with_title(&title),
        ..Default::default()
    };
    let _ = eframe::run_native(&title, options, Box::new(|cc| {
        configure_fonts(&cc.egui_ctx);
        Ok(Box::new(app))
    }));
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_RENDERER_RUN: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_renderer_Run",
    func: detra_renderer_run,
};

fn detra_renderer_pending_action(ctx: &mut ExternCallContext) -> ExternResult {
    let name = CURRENT_ACTION_NAME.with(|cell| cell.borrow().clone());
    ctx.ret_str(0, &name);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_RENDERER_PENDING_ACTION: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_renderer_PendingAction",
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
    name: "detra_renderer_PendingActionArg",
    func: detra_renderer_pending_action_arg,
};

pub fn link_detra_renderer_externs() {
    let _ = &__VO_DETRA_RENDERER_RUN;
    let _ = &__VO_DETRA_RENDERER_PENDING_ACTION;
    let _ = &__VO_DETRA_RENDERER_PENDING_ACTION_ARG;
}

vo_ext::export_extensions!();
