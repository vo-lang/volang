//! Vo egui runtime - provides extern functions for UI rendering.
//!
//! This crate implements the Rust side of the egui wrapper for Vo.
//! It uses eframe (winit + egui + wgpu) for desktop rendering.
//!
//! This is the low-level FFI layer. High-level architecture (Event/Signal)
//! is implemented in Vo (app.vo).

use std::cell::RefCell;

use eframe::egui;
use linkme::distributed_slice;
use vo_runtime::ffi::{ExternCallContext, ExternEntryWithContext, ExternResult, EXTERN_TABLE_WITH_CONTEXT};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::string;

// =============================================================================
// Thread-local state for current egui context
// =============================================================================

thread_local! {
    /// Current egui Context (set during frame callback)
    static CURRENT_CTX: RefCell<Option<egui::Context>> = const { RefCell::new(None) };
    /// Current egui Ui pointer (set during scope callbacks like Window)
    static CURRENT_UI: RefCell<Option<*mut egui::Ui>> = const { RefCell::new(None) };
}

fn with_current_ui<R>(f: impl FnOnce(&mut egui::Ui) -> R) -> Option<R> {
    CURRENT_UI.with(|cell| {
        cell.borrow().map(|ptr| {
            let ui = unsafe { &mut *ptr };
            f(ui)
        })
    })
}

fn with_current_ctx<R>(f: impl FnOnce(&egui::Context) -> R) -> Option<R> {
    CURRENT_CTX.with(|cell| {
        cell.borrow().as_ref().map(f)
    })
}

// =============================================================================
// App state for eframe
// =============================================================================

struct VoApp {
    /// Closure to call each frame (from Vo)
    frame_closure: GcRef,
    /// VM pointer for closure calls
    vm: *mut std::ffi::c_void,
    /// Fiber pointer for closure calls
    fiber: *mut std::ffi::c_void,
    /// Closure call function
    call_closure_fn: vo_runtime::ffi::ClosureCallFn,
}

impl eframe::App for VoApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Set current context for widget calls
        CURRENT_CTX.with(|cell| {
            *cell.borrow_mut() = Some(ctx.clone());
        });

        // Call the Vo frame closure
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _result = (self.call_closure_fn)(
            self.vm,
            self.fiber,
            self.frame_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );

        // Clear current context
        CURRENT_CTX.with(|cell| {
            *cell.borrow_mut() = None;
        });
    }
}

// =============================================================================
// egui_run - Main entry point
// =============================================================================

/// egui_run(title string, frame_closure func())
/// 
/// Runs the egui application with the given title and frame callback.
/// The frame_closure is called each frame to build the UI.
fn egui_run(ctx: &mut ExternCallContext) -> ExternResult {
    // arg0: title (string)
    let title_ref = ctx.arg_ref(0);
    let title = string::as_str(title_ref).to_string();
    
    // arg1: frame_closure (closure ref)
    let frame_closure = ctx.arg_ref(1);
    
    // Check if we can call closures
    if !ctx.can_call_closure() {
        return ExternResult::Panic("egui_run: closure calling not available".to_string());
    }
    
    // Get VM/fiber/call_fn from context (need to store for later use)
    // We extract these from the context before running eframe
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = ctx.closure_call_fn().unwrap();
    
    let app = VoApp {
        frame_closure,
        vm,
        fiber,
        call_closure_fn,
    };
    
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([800.0, 600.0]),
        ..Default::default()
    };
    
    // Run the eframe app (blocking)
    let _ = eframe::run_native(
        &title,
        options,
        Box::new(|_cc| Ok(Box::new(app))),
    );
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_RUN: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Run",
    func: egui_run,
};

// =============================================================================
// Window scope
// =============================================================================

/// egui_window(title string, body_closure func())
fn egui_window(ctx: &mut ExternCallContext) -> ExternResult {
    let title_ref = ctx.arg_ref(0);
    let title = string::as_str(title_ref).to_string();
    let body_closure = ctx.arg_ref(1);
    
    let egui_ctx = match with_current_ctx(|c| c.clone()) {
        Some(c) => c,
        None => return ExternResult::Panic("egui_window: no current egui context".to_string()),
    };
    
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = match ctx.closure_call_fn() {
        Some(f) => f,
        None => return ExternResult::Panic("egui_window: closure calling not available".to_string()),
    };
    
    egui::Window::new(&title).show(&egui_ctx, |ui| {
        // Set current UI for widget calls inside this scope
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = Some(ui as *mut egui::Ui);
        });
        
        // Call body closure
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _result = call_closure_fn(
            vm,
            fiber,
            body_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );
        
        // Clear current UI
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = None;
        });
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_WINDOW: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Window",
    func: egui_window,
};

// =============================================================================
// CentralPanel scope
// =============================================================================

/// egui_central_panel(body_closure func())
fn egui_central_panel(ctx: &mut ExternCallContext) -> ExternResult {
    let body_closure = ctx.arg_ref(0);
    
    let egui_ctx = match with_current_ctx(|c| c.clone()) {
        Some(c) => c,
        None => return ExternResult::Panic("egui_central_panel: no current egui context".to_string()),
    };
    
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = match ctx.closure_call_fn() {
        Some(f) => f,
        None => return ExternResult::Panic("egui_central_panel: closure calling not available".to_string()),
    };
    
    egui::CentralPanel::default().show(&egui_ctx, |ui| {
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = Some(ui as *mut egui::Ui);
        });
        
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _result = call_closure_fn(
            vm,
            fiber,
            body_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );
        
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = None;
        });
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_CENTRAL_PANEL: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_CentralPanel",
    func: egui_central_panel,
};

// =============================================================================
// Widgets
// =============================================================================

/// egui_label(text string)
fn egui_label(ctx: &mut ExternCallContext) -> ExternResult {
    let text_ref = ctx.arg_ref(0);
    let text = string::as_str(text_ref);
    
    with_current_ui(|ui| {
        ui.label(text);
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_LABEL: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Label",
    func: egui_label,
};

/// egui_button(text string) -> bool
fn egui_button(ctx: &mut ExternCallContext) -> ExternResult {
    let text_ref = ctx.arg_ref(0);
    let text = string::as_str(text_ref);
    
    let clicked = with_current_ui(|ui| {
        ui.button(text).clicked()
    }).unwrap_or(false);
    
    ctx.ret_i64(0, clicked as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_BUTTON: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Button",
    func: egui_button,
};

/// egui_heading(text string)
fn egui_heading(ctx: &mut ExternCallContext) -> ExternResult {
    let text_ref = ctx.arg_ref(0);
    let text = string::as_str(text_ref);
    
    with_current_ui(|ui| {
        ui.heading(text);
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_HEADING: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Heading",
    func: egui_heading,
};

/// egui_separator()
fn egui_separator(ctx: &mut ExternCallContext) -> ExternResult {
    let _ = ctx;
    with_current_ui(|ui| {
        ui.separator();
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_SEPARATOR: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Separator",
    func: egui_separator,
};

// =============================================================================
// Input widgets
// =============================================================================

/// egui_checkbox(label string, checked bool) -> (bool, bool)
fn egui_checkbox(ctx: &mut ExternCallContext) -> ExternResult {
    let label_ref = ctx.arg_ref(0);
    let checked_arg = ctx.arg_i64(1);
    
    let label = string::as_str(label_ref);
    let mut checked = checked_arg != 0;
    
    let changed = with_current_ui(|ui| {
        ui.checkbox(&mut checked, label).changed()
    }).unwrap_or(false);
    
    // Return (newValue, changed)
    ctx.ret_i64(0, checked as i64);
    ctx.ret_i64(1, changed as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_CHECKBOX: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Checkbox",
    func: egui_checkbox,
};

/// egui_slider_f64(label string, value float64, min float64, max float64) -> (float64, bool)
fn egui_slider_f64(ctx: &mut ExternCallContext) -> ExternResult {
    let label_ref = ctx.arg_ref(0);
    let value_arg = ctx.arg_f64(1);
    let min = ctx.arg_f64(2);
    let max = ctx.arg_f64(3);
    
    let label = string::as_str(label_ref);
    let mut value = value_arg;
    
    let changed = with_current_ui(|ui| {
        ui.add(egui::Slider::new(&mut value, min..=max).text(label)).changed()
    }).unwrap_or(false);
    
    // Return (newValue, changed)
    ctx.ret_f64(0, value);
    ctx.ret_i64(1, changed as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_SLIDER_F64: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_SliderF64",
    func: egui_slider_f64,
};

/// egui_slider_int(label string, value int, min int, max int) -> (int, bool)
fn egui_slider_int(ctx: &mut ExternCallContext) -> ExternResult {
    let label_ref = ctx.arg_ref(0);
    let value_arg = ctx.arg_i64(1);
    let min = ctx.arg_i64(2);
    let max = ctx.arg_i64(3);
    
    let label = string::as_str(label_ref);
    let mut value = value_arg;
    
    let changed = with_current_ui(|ui| {
        ui.add(egui::Slider::new(&mut value, min..=max).text(label)).changed()
    }).unwrap_or(false);
    
    // Return (newValue, changed)
    ctx.ret_i64(0, value);
    ctx.ret_i64(1, changed as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_SLIDER_INT: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_SliderInt",
    func: egui_slider_int,
};

/// egui_collapsing_header(title string, body_closure func())
fn egui_collapsing_header(ctx: &mut ExternCallContext) -> ExternResult {
    let title_ref = ctx.arg_ref(0);
    let body_closure = ctx.arg_ref(1);
    
    let title = string::as_str(title_ref);
    
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = match ctx.closure_call_fn() {
        Some(f) => f,
        None => return ExternResult::Panic("egui_collapsing_header: closure calling not available".to_string()),
    };
    
    // Get the UI pointer without holding the borrow
    let ui_ptr = CURRENT_UI.with(|cell| *cell.borrow());
    let Some(ui_ptr) = ui_ptr else {
        return ExternResult::Panic("egui_collapsing_header: no current UI".to_string());
    };
    let ui = unsafe { &mut *ui_ptr };
    
    ui.collapsing(title, |inner_ui| {
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = Some(inner_ui as *mut egui::Ui);
        });
        
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _result = call_closure_fn(
            vm,
            fiber,
            body_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );
    });
    
    // Restore parent UI
    CURRENT_UI.with(|cell| {
        *cell.borrow_mut() = Some(ui_ptr);
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_COLLAPSING_HEADER: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_CollapsingHeader",
    func: egui_collapsing_header,
};

/// egui_progress_bar(progress float64)
fn egui_progress_bar(ctx: &mut ExternCallContext) -> ExternResult {
    let progress = ctx.arg_f64(0);
    
    with_current_ui(|ui| {
        ui.add(egui::ProgressBar::new(progress as f32));
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_PROGRESS_BAR: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_ProgressBar",
    func: egui_progress_bar,
};

/// egui_spinner()
fn egui_spinner(ctx: &mut ExternCallContext) -> ExternResult {
    let _ = ctx;
    with_current_ui(|ui| {
        ui.spinner();
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_SPINNER: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Spinner",
    func: egui_spinner,
};

/// egui_space(size float64)
fn egui_space(ctx: &mut ExternCallContext) -> ExternResult {
    let size = ctx.arg_f64(0);
    
    with_current_ui(|ui| {
        ui.add_space(size as f32);
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_SPACE: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Space",
    func: egui_space,
};

/// egui_indent(indent float64, body_closure func())
fn egui_indent(ctx: &mut ExternCallContext) -> ExternResult {
    let indent = ctx.arg_f64(0);
    let body_closure = ctx.arg_ref(1);
    
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = match ctx.closure_call_fn() {
        Some(f) => f,
        None => return ExternResult::Panic("egui_indent: closure calling not available".to_string()),
    };
    
    // Get the UI pointer without holding the borrow
    let ui_ptr = CURRENT_UI.with(|cell| *cell.borrow());
    let Some(ui_ptr) = ui_ptr else {
        return ExternResult::Panic("egui_indent: no current UI".to_string());
    };
    let ui = unsafe { &mut *ui_ptr };
    
    ui.indent("indent", |inner_ui| {
        inner_ui.add_space(indent as f32 - 18.0); // Adjust for default indent
        
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = Some(inner_ui as *mut egui::Ui);
        });
        
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _result = call_closure_fn(
            vm,
            fiber,
            body_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );
    });
    
    // Restore parent UI
    CURRENT_UI.with(|cell| {
        *cell.borrow_mut() = Some(ui_ptr);
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_INDENT: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Indent",
    func: egui_indent,
};

// =============================================================================
// Layout helpers
// =============================================================================

/// egui_horizontal(body_closure func())
fn egui_horizontal(ctx: &mut ExternCallContext) -> ExternResult {
    let body_closure = ctx.arg_ref(0);
    
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = match ctx.closure_call_fn() {
        Some(f) => f,
        None => return ExternResult::Panic("egui_horizontal: closure calling not available".to_string()),
    };
    
    // Get the UI pointer without holding the borrow
    let ui_ptr = CURRENT_UI.with(|cell| *cell.borrow());
    let Some(ui_ptr) = ui_ptr else {
        return ExternResult::Panic("egui_horizontal: no current UI".to_string());
    };
    let ui = unsafe { &mut *ui_ptr };
    
    ui.horizontal(|inner_ui| {
        // Set inner UI as current
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = Some(inner_ui as *mut egui::Ui);
        });
        
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _result = call_closure_fn(
            vm,
            fiber,
            body_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );
    });
    
    // Restore parent UI
    CURRENT_UI.with(|cell| {
        *cell.borrow_mut() = Some(ui_ptr);
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_HORIZONTAL: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Horizontal",
    func: egui_horizontal,
};

/// egui_vertical(body_closure func())
fn egui_vertical(ctx: &mut ExternCallContext) -> ExternResult {
    let body_closure = ctx.arg_ref(0);
    
    let vm = ctx.vm_ptr();
    let fiber = ctx.fiber_ptr();
    let call_closure_fn = match ctx.closure_call_fn() {
        Some(f) => f,
        None => return ExternResult::Panic("egui_vertical: closure calling not available".to_string()),
    };
    
    // Get the UI pointer without holding the borrow
    let ui_ptr = CURRENT_UI.with(|cell| *cell.borrow());
    let Some(ui_ptr) = ui_ptr else {
        return ExternResult::Panic("egui_vertical: no current UI".to_string());
    };
    let ui = unsafe { &mut *ui_ptr };
    
    ui.vertical(|inner_ui| {
        // Set inner UI as current
        CURRENT_UI.with(|cell| {
            *cell.borrow_mut() = Some(inner_ui as *mut egui::Ui);
        });
        
        let args: [u64; 0] = [];
        let mut ret: [u64; 0] = [];
        let _result = call_closure_fn(
            vm,
            fiber,
            body_closure as u64,
            args.as_ptr(),
            0,
            ret.as_mut_ptr(),
            0,
        );
    });
    
    // Restore parent UI
    CURRENT_UI.with(|cell| {
        *cell.borrow_mut() = Some(ui_ptr);
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_VERTICAL: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_Vertical",
    func: egui_vertical,
};

// =============================================================================
// Theme
// =============================================================================

/// egui_set_dark_mode(dark bool)
fn egui_set_dark_mode(ctx: &mut ExternCallContext) -> ExternResult {
    let dark = ctx.arg_i64(0) != 0;
    
    with_current_ctx(|egui_ctx| {
        if dark {
            egui_ctx.set_visuals(egui::Visuals::dark());
        } else {
            egui_ctx.set_visuals(egui::Visuals::light());
        }
    });
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_EGUI_SET_DARK_MODE: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_vui_SetDarkMode",
    func: egui_set_dark_mode,
};

// =============================================================================
// Public API for linking
// =============================================================================

/// Call this to ensure the egui externs are linked into the binary.
/// Without this, the linker may strip the extern registrations.
pub fn link_egui_externs() {
    // Force linkme to include the distributed slices
    let _ = &__VO_EGUI_RUN;
    let _ = &__VO_EGUI_WINDOW;
    let _ = &__VO_EGUI_CENTRAL_PANEL;
    let _ = &__VO_EGUI_LABEL;
    let _ = &__VO_EGUI_BUTTON;
    let _ = &__VO_EGUI_HEADING;
    let _ = &__VO_EGUI_SEPARATOR;
    let _ = &__VO_EGUI_HORIZONTAL;
    let _ = &__VO_EGUI_VERTICAL;
}

// Export extension entry point for dynamic loading
vo_ext::export_extensions!();
