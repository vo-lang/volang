# Tauri Wrapper (`libs/vo-tauri`) — Detailed Design

> Date: 2026-02-22  
> Parent: `2026-02-22_00_overview.md`

---

## 1. Purpose

A reusable Rust crate that bridges the Vo engine to any Tauri application. It provides:

- Tauri IPC commands for compiling, running, and interacting with Vo GUI apps
- VM lifecycle management (init, event handling, teardown)
- vogui platform implementation (timers, navigation forwarded to webview)
- Native extension registration (bundled, no WASM)

Any Tauri-based Vo app (playground, custom IDE, game editor) depends on this crate.

---

## 2. Crate Structure

```
libs/vo-tauri/
├── Cargo.toml
└── src/
    ├── lib.rs              # Public API: register_commands(), VoTauriState
    ├── commands.rs         # #[tauri::command] handlers
    ├── gui_state.rs        # GuiAppState: VM + event handler lifecycle
    └── platform.rs         # TauriPlatform: VoguiPlatform impl
```

### 2.1 Cargo.toml

```toml
[package]
name = "vo-tauri"
version = "0.1.0"
edition = "2021"

[dependencies]
tauri = { version = "2", features = ["protocol-asset"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"

# Vo engine
vo-engine = { path = "../../lang/crates/vo-engine" }
vo-vm = { path = "../../lang/crates/vo-vm" }
vo-runtime = { path = "../../lang/crates/vo-runtime" }
vo-stdlib = { path = "../../lang/crates/vo-stdlib" }
vo-common = { path = "../../lang/crates/vo-common" }

# GUI
vogui = { path = "../vogui/rust", package = "vo-gui" }
```

---

## 3. Public API (`lib.rs`)

```rust
use tauri::{AppHandle, Manager};
use std::sync::Mutex;

mod commands;
mod gui_state;
mod platform;

/// Shared state managed by Tauri.
pub struct VoTauriState {
    pub gui: Mutex<Option<gui_state::GuiAppState>>,
}

/// Call this from your Tauri app's setup to register all Vo commands.
pub fn register_commands(builder: tauri::Builder<tauri::Wry>) -> tauri::Builder<tauri::Wry> {
    builder
        .manage(VoTauriState {
            gui: Mutex::new(None),
        })
        .invoke_handler(tauri::generate_handler![
            commands::compile_and_run,
            commands::init_gui_app,
            commands::init_gui_app_with_modules,
            commands::handle_gui_event,
            commands::reset_gui,
        ])
}

pub use platform::TauriPlatform;
pub use gui_state::GuiAppState;
```

---

## 4. IPC Commands (`commands.rs`)

### 4.1 Return Types

```rust
#[derive(serde::Serialize, Clone)]
pub struct GuiResult {
    pub status: String,       // "ok" | "error" | "compile_error"
    pub render_json: String,  // vogui node tree JSON
    pub error: String,        // error message (empty on success)
}

#[derive(serde::Serialize, Clone)]
pub struct RunResult {
    pub status: String,
    pub stdout: String,
    pub stderr: String,
}
```

### 4.2 compile_and_run

Compiles and runs a non-GUI Vo program. Returns stdout/stderr.

```rust
#[tauri::command]
pub fn compile_and_run(
    source: String,
    mode: Option<String>,  // "vm" | "jit", default "vm"
) -> Result<RunResult, String> {
    let run_mode = match mode.as_deref() {
        Some("jit") => vo_engine::RunMode::Jit,
        _ => vo_engine::RunMode::Vm,
    };

    // Compile
    let output = vo_engine::compile_string(&source, "main.vo")
        .map_err(|e| format!("{}", e))?;

    // Capture stdout
    vo_runtime::output::clear_output();

    // Run
    match vo_engine::run(output, run_mode, Vec::new()) {
        Ok(()) => Ok(RunResult {
            status: "ok".into(),
            stdout: vo_runtime::output::take_output(),
            stderr: String::new(),
        }),
        Err(e) => Ok(RunResult {
            status: "error".into(),
            stdout: vo_runtime::output::take_output(),
            stderr: format!("{}", e),
        }),
    }
}
```

### 4.3 init_gui_app

Compiles vogui source and initializes a GUI app. VM stays alive for event handling.

```rust
#[tauri::command]
pub fn init_gui_app(
    source: String,
    state: tauri::State<'_, VoTauriState>,
    app_handle: AppHandle,
) -> Result<GuiResult, String> {
    // Set up Tauri platform for vogui (timers → webview events)
    vogui::set_platform(Box::new(TauriPlatform::new(app_handle.clone())));

    // Build filesystem: stdlib + vogui
    let mut fs = build_stdlib_fs();
    add_vogui_to_fs(&mut fs);

    // Compile
    let bytecode = compile_source(&source, "main.vo", fs)?;

    // Run and extract GUI state
    let gui_result = gui_state::run_gui_bytecode(
        &bytecode,
        register_gui_externs,
    )?;

    // Store state for subsequent handle_gui_event calls
    *state.gui.lock().unwrap() = Some(gui_result.state);

    Ok(GuiResult {
        status: "ok".into(),
        render_json: gui_result.render_json,
        error: String::new(),
    })
}
```

### 4.4 init_gui_app_with_modules

Same as `init_gui_app` but additionally registers bundled extensions (wgpu, etc.):

```rust
#[tauri::command]
pub fn init_gui_app_with_modules(
    source: String,
    state: tauri::State<'_, VoTauriState>,
    app_handle: AppHandle,
) -> Result<GuiResult, String> {
    vogui::set_platform(Box::new(TauriPlatform::new(app_handle.clone())));

    let mut fs = build_stdlib_fs();
    add_vogui_to_fs(&mut fs);

    // Detect and load module source files
    let (mod_fs, clean_source) = prepare_modules(&source)?;

    let bytecode = compile_source_with_mods(&clean_source, "main.vo", fs, mod_fs)?;

    let gui_result = gui_state::run_gui_bytecode(
        &bytecode,
        register_gui_and_ext_externs,  // vogui + wgpu + other bundled exts
    )?;

    *state.gui.lock().unwrap() = Some(gui_result.state);

    Ok(GuiResult {
        status: "ok".into(),
        render_json: gui_result.render_json,
        error: String::new(),
    })
}

fn register_gui_and_ext_externs(reg: &mut ExternRegistry, externs: &[ExternDef]) {
    vogui::register_externs(reg, externs);
    vo_wgpu::register_externs(reg, externs);  // Native wgpu, statically linked
}
```

### 4.5 handle_gui_event

Handles a user interaction event from the webview (click, timer, resize, etc.):

```rust
#[tauri::command]
pub fn handle_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, VoTauriState>,
) -> Result<GuiResult, String> {
    let mut guard = state.gui.lock().unwrap();
    let gui = guard.as_mut().ok_or("GUI app not initialized")?;

    let result = gui.handle_event(handler_id, &payload)?;

    Ok(GuiResult {
        status: "ok".into(),
        render_json: result.render_json,
        error: String::new(),
    })
}
```

### 4.6 reset_gui

Tears down the current GUI app state:

```rust
#[tauri::command]
pub fn reset_gui(state: tauri::State<'_, VoTauriState>) -> Result<(), String> {
    *state.gui.lock().unwrap() = None;
    Ok(())
}
```

---

## 5. VM Lifecycle (`gui_state.rs`)

Mirrors the logic in `playground/rust/src/lib.rs` but without WASM dependencies.

```rust
use vo_vm::vm::Vm;
use vo_runtime::ffi::{ExternRegistry, ExternCallContext, ExternResult};
use vo_runtime::gc::GcRef;
use vo_vm::bytecode::{Module, ExternDef};

pub type ExternRegistrar = fn(&mut ExternRegistry, &[ExternDef]);

pub struct GuiAppState {
    vm: Vm,
    event_handler: GcRef,
}

pub struct GuiInitResult {
    pub state: GuiAppState,
    pub render_json: String,
}

pub struct EventResult {
    pub render_json: String,
}

pub fn run_gui_bytecode(
    bytecode: &[u8],
    registrar: ExternRegistrar,
) -> Result<GuiInitResult, String> {
    let module = Module::deserialize(bytecode)
        .map_err(|e| format!("Failed to load bytecode: {:?}", e))?;

    vo_runtime::output::clear_output();

    let mut vm = Vm::new();
    // Register stdlib
    vo_stdlib::register_externs(&mut vm.state.extern_registry, &module.externs);
    // Register caller externs (vogui, wgpu, etc.)
    registrar(&mut vm.state.extern_registry, &module.externs);

    vm.load(module);
    vm.run().map_err(|e| format!("{:?}", e))?;

    // Flush stdout
    let stdout = vo_runtime::output::take_output();
    if !stdout.is_empty() {
        eprintln!("[Vo] {}", stdout);  // Or forward to webview
    }

    // Extract render JSON
    let render_json = vogui::take_pending_render()
        .ok_or("No render output. emitRender was not called.")?;

    // Extract event handler
    let event_handler = vogui::take_pending_handler()
        .ok_or("registerEventHandler not called")?;

    Ok(GuiInitResult {
        state: GuiAppState { vm, event_handler },
        render_json,
    })
}

impl GuiAppState {
    pub fn handle_event(&mut self, handler_id: i32, payload: &str) -> Result<EventResult, String> {
        vo_runtime::output::clear_output();

        let payload_ref = vo_runtime::objects::string::from_rust_str(
            &mut self.vm.state.gc, payload
        );

        // Build closure args and call
        let func_id = vo_runtime::objects::closure::func_id(self.event_handler);
        let module = self.vm.module().expect("module not set");
        let func_def = &module.functions[func_id as usize];

        let args = [handler_id as u64, payload_ref as u64];
        let full_args = vo_vm::vm::helpers::build_closure_args(
            self.event_handler as u64,
            self.event_handler,
            func_def,
            args.as_ptr(),
            args.len() as u32,
        );

        self.vm.spawn_call(func_id, &full_args);
        self.vm.run_scheduled().map_err(|e| format!("{:?}", e))?;

        let render_json = vogui::take_pending_render().unwrap_or_default();
        Ok(EventResult { render_json })
    }
}
```

---

## 6. vogui Platform Trait Refactor

### 6.1 Current State (to be refactored)

In `libs/vogui/rust/src/lib.rs`:
- `#[cfg(target_arch = "wasm32")] mod js { ... }` — wasm_bindgen extern imports
- `#[cfg(not(target_arch = "wasm32"))] mod native_stubs { ... }` — no-op functions

### 6.2 New Design

```rust
// libs/vogui/rust/src/lib.rs

use std::cell::RefCell;

pub trait VoguiPlatform: 'static {
    fn start_timeout(&self, id: i32, ms: i32);
    fn clear_timeout(&self, id: i32);
    fn start_interval(&self, id: i32, ms: i32);
    fn clear_interval(&self, id: i32);
    fn navigate(&self, path: &str);
    fn get_current_path(&self) -> String;
}

thread_local! {
    static PLATFORM: RefCell<Option<Box<dyn VoguiPlatform>>> = RefCell::new(None);
}

pub fn set_platform(platform: Box<dyn VoguiPlatform>) {
    PLATFORM.with(|p| *p.borrow_mut() = Some(platform));
}

// Internal dispatch (used by extern impls)
pub(crate) fn with_platform<F, R>(f: F, default: R) -> R
where F: FnOnce(&dyn VoguiPlatform) -> R {
    PLATFORM.with(|p| {
        match p.borrow().as_ref() {
            Some(platform) => f(platform.as_ref()),
            None => default,
        }
    })
}
```

### 6.3 WasmPlatform (for web playground)

```rust
// libs/vogui/rust/src/wasm_platform.rs
#[cfg(target_arch = "wasm32")]
pub struct WasmPlatform;

#[cfg(target_arch = "wasm32")]
mod js_imports {
    use wasm_bindgen::prelude::*;
    #[wasm_bindgen] extern "C" {
        #[wasm_bindgen(js_name = startTimeout)] pub fn start_timeout(id: i32, ms: i32);
        #[wasm_bindgen(js_name = clearTimeout)] pub fn clear_timeout(id: i32);
        #[wasm_bindgen(js_name = startInterval)] pub fn start_interval(id: i32, ms: i32);
        #[wasm_bindgen(js_name = clearInterval)] pub fn clear_interval(id: i32);
        #[wasm_bindgen(js_name = navigate)] pub fn navigate(path: &str);
        #[wasm_bindgen(js_name = getCurrentPath)] pub fn get_current_path() -> String;
    }
}

#[cfg(target_arch = "wasm32")]
impl VoguiPlatform for WasmPlatform {
    fn start_timeout(&self, id: i32, ms: i32) { js_imports::start_timeout(id, ms); }
    fn clear_timeout(&self, id: i32) { js_imports::clear_timeout(id); }
    fn start_interval(&self, id: i32, ms: i32) { js_imports::start_interval(id, ms); }
    fn clear_interval(&self, id: i32) { js_imports::clear_interval(id); }
    fn navigate(&self, path: &str) { js_imports::navigate(path); }
    fn get_current_path(&self) -> String { js_imports::get_current_path() }
}
```

### 6.4 TauriPlatform (in vo-tauri)

```rust
// libs/vo-tauri/src/platform.rs
use tauri::AppHandle;
use vogui::VoguiPlatform;

pub struct TauriPlatform {
    app_handle: AppHandle,
}

impl TauriPlatform {
    pub fn new(app_handle: AppHandle) -> Self {
        Self { app_handle }
    }
}

impl VoguiPlatform for TauriPlatform {
    fn start_timeout(&self, id: i32, ms: i32) {
        self.app_handle.emit("vo-start-timeout", (id, ms)).ok();
    }
    fn clear_timeout(&self, id: i32) {
        self.app_handle.emit("vo-clear-timeout", id).ok();
    }
    fn start_interval(&self, id: i32, ms: i32) {
        self.app_handle.emit("vo-start-interval", (id, ms)).ok();
    }
    fn clear_interval(&self, id: i32) {
        self.app_handle.emit("vo-clear-interval", id).ok();
    }
    fn navigate(&self, path: &str) {
        self.app_handle.emit("vo-navigate", path).ok();
    }
    fn get_current_path(&self) -> String {
        // For Tauri, route is managed by the webview.
        // We could use a shared state or synchronous IPC.
        // For now, "/" is acceptable since playground doesn't use routing.
        "/".to_string()
    }
}
```

### 6.5 Updated Extern Dispatch

```rust
// libs/vogui/rust/src/externs.rs (updated)

#[vo_fn("vogui", "startTimeout")]
pub fn start_timeout(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    let ms = ctx.arg_i64(slots::ARG_MS) as i32;
    crate::with_platform(|p| p.start_timeout(id, ms), ());
    ExternResult::Ok
}

// Same pattern for clearTimeout, startInterval, clearInterval, navigate, getCurrentPath
```

---

## 7. Module Source Resolution (Native)

On the web, third-party module `.vo` source files are fetched from GitHub. On native, two options:

### 7.1 Bundled (for playground)

vogui `.vo` files are already embedded at build time via `include_dir!`. For wgpu, the same approach:

```rust
// apps/playground-desktop/src-tauri/build.rs
// Embed wgpu .vo files just like vogui .vo files
const WGPU_FILES: &[(&str, &[u8])] = &[
    ("github.com/vo-lang/wgpu/wgpu.vo", include_bytes!("../../../3rdparty/wgpu/wgpu.vo")),
    ("github.com/vo-lang/wgpu/types.vo", include_bytes!("../../../3rdparty/wgpu/types.vo")),
];
```

### 7.2 Downloaded (for standalone CLI)

`vo get github.com/vo-lang/wgpu@v0.2.0` downloads:
- `.vo` source files → `~/.vo/pkg/github.com/vo-lang/wgpu@v0.2.0/`
- Native shared library → `~/.vo/pkg/github.com/vo-lang/wgpu@v0.2.0/libvo_wgpu.{so,dylib,dll}`

The `vo.ext.toml` manifest tells `ExtensionLoader` where to find the native lib:

```toml
[extension]
name = "github.com/vo-lang/wgpu"
type = "native"
path = "libvo_wgpu"

[extension.wasm]
type = "bindgen"
js_glue = "wgpu.js"
wasm = "wgpu.wasm"
```

---

## 8. Webview JS: Timer & Event Bridge

The Tauri webview needs JS to handle vogui platform events:

```typescript
// apps/playground-desktop/src/lib/tauri-bridge.ts
import { listen } from '@tauri-apps/api/event';
import { invoke } from '@tauri-apps/api/core';

const timers = new Map<number, number>();  // id → browser timer handle

export function setupVoguiBridge(onRender: (json: string) => void) {
    listen('vo-start-timeout', (event) => {
        const { id, ms } = event.payload as { id: number; ms: number };
        const handle = window.setTimeout(async () => {
            timers.delete(id);
            const result = await invoke<GuiResult>('handle_gui_event', {
                handlerId: id,
                payload: '{"type":"timeout"}',
            });
            if (result.render_json) onRender(result.render_json);
        }, ms);
        timers.set(id, handle);
    });

    listen('vo-clear-timeout', (event) => {
        const id = event.payload as number;
        const handle = timers.get(id);
        if (handle !== undefined) {
            window.clearTimeout(handle);
            timers.delete(id);
        }
    });

    listen('vo-start-interval', (event) => {
        const { id, ms } = event.payload as { id: number; ms: number };
        const handle = window.setInterval(async () => {
            const result = await invoke<GuiResult>('handle_gui_event', {
                handlerId: id,
                payload: '{"type":"interval"}',
            });
            if (result.render_json) onRender(result.render_json);
        }, ms);
        timers.set(id, handle);
    });

    listen('vo-clear-interval', (event) => {
        const id = event.payload as number;
        const handle = timers.get(id);
        if (handle !== undefined) {
            window.clearInterval(handle);
            timers.delete(id);
        }
    });
}

export function teardownVoguiBridge() {
    for (const [id, handle] of timers) {
        window.clearTimeout(handle);
        window.clearInterval(handle);
    }
    timers.clear();
}
```

---

## 9. JIT Support

The native playground supports JIT, which the web playground cannot:

```rust
// In gui_state.rs, optionally enable JIT:
pub fn run_gui_bytecode_jit(
    bytecode: &[u8],
    registrar: ExternRegistrar,
) -> Result<GuiInitResult, String> {
    // ... same as run_gui_bytecode but with:
    let mut vm = {
        let config = vo_vm::JitConfig {
            call_threshold: 100,
            loop_threshold: 50,
            debug_ir: false,
        };
        let mut vm = Vm::with_jit_config(config);
        vm.init_jit();
        vm
    };
    // ... rest is identical
}
```

The Tauri command can accept a `mode` parameter:

```rust
#[tauri::command]
pub fn init_gui_app(source: String, mode: Option<String>, ...) -> Result<GuiResult, String> {
    let use_jit = mode.as_deref() == Some("jit");
    // ...
}
```

---

## 10. Error Handling

All Tauri commands return `Result<T, String>`. The error string is displayed in the webview's output panel.

For panics inside the VM:
- `vo_runtime::output::take_output()` captures any println before the panic
- `VmError` is formatted with source location via `debug_info.lookup()`
- The GuiResult includes both the error message and any partial stdout

For wgpu errors:
- Native wgpu errors are propagated through the same `Result<T, String>` chain
- The Vo code receives them as standard `error` return values

---

## 11. Testing

### Unit Tests (`cargo test -p vo-tauri`)
- `test_gui_result_serialization` — JSON round-trip
- `test_noop_platform` — NoopPlatform doesn't panic
- `test_run_simple_program` — compile_and_run("println(42)") → stdout "42"

### Integration Tests
- Start Tauri app in test mode
- `invoke("compile_and_run", { source: 'println("hello")' })` → check stdout
- `invoke("init_gui_app", { source: COUNTER_APP })` → check render_json has nodes
- `invoke("handle_gui_event", { handlerId: 1, payload: '...' })` → check updated render
