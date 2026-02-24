# Native Playground (`apps/playground-desktop`) — Detailed Design

> Date: 2026-02-22  
> Parent: `2026-02-22_00_overview.md`  
> Depends on: `2026-02-22_01_tauri_wrapper.md`

---

## 1. Purpose

A desktop application that provides the same experience as the web playground but runs the Vo engine natively:

| Feature | Web Playground | Native Playground |
|---------|---------------|-------------------|
| Compilation speed | WASM (slow) | Native (fast) |
| JIT support | ✗ | ✓ |
| Filesystem access | ✗ | ✓ (local .vo files) |
| Ext module loading | WASM instantiate | Statically linked |
| wgpu rendering | WebGPU in browser | Native wgpu crate |
| UI renderer | vogui JS → DOM | vogui JS → webview DOM |
| Editor | CodeMirror/Monaco | Same (in webview) |
| Offline usage | ✗ (needs CDN) | ✓ |

---

## 2. Directory Structure

```
apps/playground-desktop/
├── src-tauri/
│   ├── Cargo.toml
│   ├── tauri.conf.json
│   ├── icons/                    # App icons
│   ├── build.rs                  # Embed vogui + wgpu .vo files
│   └── src/
│       └── main.rs               # Tauri app entry point
├── src/                          # Svelte frontend
│   ├── App.svelte                # Root component
│   ├── main.ts                   # Entry point
│   ├── app.css                   # Global styles
│   ├── components/
│   │   ├── Editor.svelte         # Code editor (shared)
│   │   ├── GuiPreview.svelte     # GUI preview (shared)
│   │   ├── Output.svelte         # Console output (shared)
│   │   └── Toolbar.svelte        # Run/Stop buttons
│   ├── lib/
│   │   ├── platform.ts           # VoPlatform interface
│   │   ├── wasm-platform.ts      # Web impl (unused in desktop, for sharing)
│   │   ├── tauri-platform.ts     # Tauri IPC impl
│   │   └── tauri-bridge.ts       # Timer/event bridge (vogui → webview)
│   └── assets/
│       └── examples/             # Example .vo programs (embedded or loaded)
├── package.json
├── svelte.config.js
├── vite.config.ts
└── tsconfig.json
```

---

## 3. Cargo Dependencies (`src-tauri/Cargo.toml`)

```toml
[package]
name = "vo-playground-desktop"
version = "0.1.0"
edition = "2021"

[build-dependencies]
tauri-build = "2"

[dependencies]
tauri = { version = "2", features = [] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"

# Vo core
vo-tauri = { path = "../../../libs/vo-tauri" }
vo-engine = { path = "../../../lang/crates/vo-engine", features = ["jit"] }

# GUI
vo-gui = { path = "../../../libs/vogui/rust" }

# GPU (native backend)
vo-wgpu = { path = "../../../3rdparty/wgpu/rust", features = ["native"] }
```

---

## 4. Tauri App Entry Point (`src-tauri/src/main.rs`)

```rust
fn main() {
    // Ensure linked: force linker to include FFI symbols
    vo_wgpu::ensure_linked();

    let builder = tauri::Builder::default();
    let builder = vo_tauri::register_commands(builder);

    builder
        .setup(|app| {
            // Set platform for vogui
            let handle = app.handle().clone();
            vogui::set_platform(Box::new(vo_tauri::TauriPlatform::new(handle)));
            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error running Vo Playground");
}
```

---

## 5. Tauri Configuration (`tauri.conf.json`)

```json
{
  "$schema": "https://raw.githubusercontent.com/nicegram/nicegram/refs/heads/main/nicegram-manager/src-tauri/gen/schemas/desktop-schema.json",
  "productName": "Vo Playground",
  "version": "0.1.0",
  "identifier": "com.volang.playground",
  "build": {
    "frontendDist": "../dist",
    "devUrl": "http://localhost:5173",
    "beforeDevCommand": "npm run dev",
    "beforeBuildCommand": "npm run build"
  },
  "app": {
    "title": "Vo Playground",
    "windows": [
      {
        "title": "Vo Playground",
        "width": 1200,
        "height": 800,
        "resizable": true
      }
    ],
    "security": {
      "csp": null
    }
  }
}
```

---

## 6. Build Script (`build.rs`)

Embeds vogui `.vo` files and wgpu `.vo` files at compile time:

```rust
fn main() {
    tauri_build::build();

    // Ensure rebuild when .vo files change
    println!("cargo:rerun-if-changed=../../../libs/vogui");
    println!("cargo:rerun-if-changed=../../../3rdparty/wgpu/wgpu.vo");
    println!("cargo:rerun-if-changed=../../../3rdparty/wgpu/types.vo");
}
```

The actual file embedding is done in `vo-tauri` or the app's `main.rs` using `include_bytes!` or `include_dir!`, same pattern as `playground/rust/build.rs`.

---

## 7. Frontend: Platform Abstraction

### 7.1 Interface (`lib/platform.ts`)

```typescript
export interface GuiResult {
    status: string;
    render_json: string;
    error: string;
}

export interface RunResult {
    status: string;
    stdout: string;
    stderr: string;
}

export interface VoPlatform {
    /** Compile and run a non-GUI program */
    compileAndRun(source: string): Promise<RunResult>;

    /** Initialize a GUI app (compile + first render) */
    initGuiApp(source: string): Promise<GuiResult>;

    /** Initialize a GUI app that imports third-party modules */
    initGuiAppWithModules(source: string): Promise<GuiResult>;

    /** Handle a GUI event (returns updated render) */
    handleGuiEvent(handlerId: number, payload: string): Promise<GuiResult>;

    /** Reset/teardown the current GUI app */
    reset(): Promise<void>;

    /** Whether this platform supports JIT mode */
    readonly supportsJit: boolean;

    /** Platform name for display */
    readonly name: string;
}
```

### 7.2 Tauri Implementation (`lib/tauri-platform.ts`)

```typescript
import { invoke } from '@tauri-apps/api/core';
import type { VoPlatform, GuiResult, RunResult } from './platform';

export class TauriPlatform implements VoPlatform {
    readonly supportsJit = true;
    readonly name = 'Native (Tauri)';

    async compileAndRun(source: string): Promise<RunResult> {
        return await invoke<RunResult>('compile_and_run', { source });
    }

    async initGuiApp(source: string): Promise<GuiResult> {
        return await invoke<GuiResult>('init_gui_app', { source });
    }

    async initGuiAppWithModules(source: string): Promise<GuiResult> {
        return await invoke<GuiResult>('init_gui_app_with_modules', { source });
    }

    async handleGuiEvent(handlerId: number, payload: string): Promise<GuiResult> {
        return await invoke<GuiResult>('handle_gui_event', {
            handlerId,
            payload,
        });
    }

    async reset(): Promise<void> {
        await invoke('reset_gui');
    }
}
```

### 7.3 WASM Implementation (`lib/wasm-platform.ts`)

Wraps the existing `vo.ts` functions for the web playground:

```typescript
import {
    initGuiApp as wasmInitGuiApp,
    initGuiAppWithModules as wasmInitGuiAppWithModules,
    handleGuiEvent as wasmHandleGuiEvent,
} from '../wasm/vo';
import type { VoPlatform, GuiResult, RunResult } from './platform';

export class WasmPlatform implements VoPlatform {
    readonly supportsJit = false;
    readonly name = 'Web (WASM)';

    async compileAndRun(source: string): Promise<RunResult> {
        // Existing compileAndRun logic from vo.ts
        // ...
    }

    async initGuiApp(source: string): Promise<GuiResult> {
        return await wasmInitGuiApp(source);
    }

    // ...
}
```

### 7.4 Platform Detection (`App.svelte`)

```svelte
<script lang="ts">
import { setContext, onMount } from 'svelte';

let platform;

onMount(async () => {
    if (window.__TAURI_INTERNALS__) {
        const { TauriPlatform } = await import('./lib/tauri-platform');
        const { setupVoguiBridge } = await import('./lib/tauri-bridge');
        platform = new TauriPlatform();
        setupVoguiBridge((json) => { /* update GUI preview */ });
    } else {
        const { WasmPlatform } = await import('./lib/wasm-platform');
        platform = new WasmPlatform();
    }
    setContext('platform', platform);
});
</script>
```

---

## 8. Shared vs Forked Frontend Code

### Strategy: Shared source, build-time config

The Svelte source lives in one place. Both web and desktop builds use the same source with different entry points and Vite configs.

```
playground/src/           # Web playground frontend (existing)
apps/playground-desktop/src/  # Desktop frontend

# Option A: Symlink shared components
apps/playground-desktop/src/components → ../../playground/src/components

# Option B: Monorepo package
# Extract shared components into a package that both consume

# Option C: Copy at build time
# build.rs or npm script copies shared files
```

**Recommended: Option A (symlinks)** for simplicity. The shared components are:
- `Editor.svelte` — code editor
- `GuiPreview.svelte` — vogui renderer (needs minor adaptation for platform)
- `Output.svelte` — console output display

Desktop-specific:
- `App.svelte` — different layout, file open dialog, JIT toggle
- `Toolbar.svelte` — includes "Open File", "Save", JIT mode switch
- `tauri-bridge.ts` — timer/event forwarding

### 8.1 GuiPreview Adaptation

The current `GuiPreview.svelte` calls WASM functions directly. Refactor to use `VoPlatform`:

```svelte
<script lang="ts">
import { getContext } from 'svelte';
import type { VoPlatform } from '../lib/platform';

const platform = getContext<VoPlatform>('platform');

async function runApp(source: string) {
    const hasModules = source.includes('import "github.com/');
    const result = hasModules
        ? await platform.initGuiAppWithModules(source)
        : await platform.initGuiApp(source);

    if (result.status === 'ok') {
        renderVoguiJson(result.render_json);
    } else {
        showError(result.error);
    }
}
</script>
```

---

## 9. Canvas & wgpu Native Surface

### 9.1 The Problem

On web: vogui Canvas → HTML `<canvas>` → WebGPU context → render.
On native (Tauri): vogui Canvas → HTML `<canvas>` in webview → but wgpu runs natively, not in webview.

### 9.2 Solution: Native Child Window Overlay

```
Step 1: Webview renders Canvas placeholder
        └─→ <div id="vo-canvas-gpu" style="width:800px;height:600px">
              <!-- transparent placeholder -->
            </div>

Step 2: JS reports placeholder position to Rust backend
        └─→ invoke('register_canvas_surface', {
              id: 'gpu',
              x: 100, y: 50,       // absolute position in window
              width: 800, height: 600
            })

Step 3: Rust creates a native child window at that position
        └─→ raw-window-handle → wgpu::Instance::create_surface()

Step 4: wgpu renders to the native surface (overlay on webview)

Step 5: On resize/scroll, JS sends updated position
        └─→ invoke('update_canvas_surface', { id: 'gpu', x, y, w, h })
```

### 9.3 Canvas Surface Manager (Rust)

```rust
// In vo-tauri or the desktop app

use std::collections::HashMap;
use std::sync::Mutex;

pub struct CanvasSurfaceManager {
    surfaces: HashMap<String, NativeSurface>,
}

struct NativeSurface {
    // Platform-specific child window handle
    #[cfg(target_os = "linux")]
    window: /* X11/Wayland child window */,
    #[cfg(target_os = "macos")]
    window: /* NSView child */,
    #[cfg(target_os = "windows")]
    window: /* HWND child */,

    surface: wgpu::Surface<'static>,
    config: wgpu::SurfaceConfiguration,
}

#[tauri::command]
fn register_canvas_surface(
    id: String, x: i32, y: i32, width: u32, height: u32,
    window: tauri::Window,
    state: tauri::State<'_, Mutex<CanvasSurfaceManager>>,
) -> Result<(), String> {
    let mut mgr = state.lock().unwrap();
    mgr.create_surface(&id, &window, x, y, width, height)
}

#[tauri::command]
fn update_canvas_surface(
    id: String, x: i32, y: i32, width: u32, height: u32,
    state: tauri::State<'_, Mutex<CanvasSurfaceManager>>,
) -> Result<(), String> {
    let mut mgr = state.lock().unwrap();
    mgr.resize_surface(&id, x, y, width, height)
}
```

### 9.4 Webview Canvas Observer

```typescript
// In tauri-bridge.ts

export function observeCanvasPlaceholders() {
    const observer = new MutationObserver(() => {
        document.querySelectorAll('.vo-canvas').forEach((el) => {
            const rect = el.getBoundingClientRect();
            const id = el.id.replace('vo-canvas-', '');
            invoke('register_canvas_surface', {
                id,
                x: Math.round(rect.left),
                y: Math.round(rect.top),
                width: Math.round(rect.width),
                height: Math.round(rect.height),
            });
        });
    });
    observer.observe(document.body, { childList: true, subtree: true });

    // Also observe resize
    const resizeObserver = new ResizeObserver((entries) => {
        for (const entry of entries) {
            const el = entry.target as HTMLElement;
            if (el.classList.contains('vo-canvas')) {
                const rect = el.getBoundingClientRect();
                const id = el.id.replace('vo-canvas-', '');
                invoke('update_canvas_surface', {
                    id,
                    x: Math.round(rect.left),
                    y: Math.round(rect.top),
                    width: Math.round(rect.width),
                    height: Math.round(rect.height),
                });
            }
        }
    });

    return { observer, resizeObserver };
}
```

### 9.5 Fallback: Separate Window

If child window overlay proves too complex for a platform, fall back to opening a separate native window:

```rust
fn create_surface_fallback(id: &str, width: u32, height: u32) -> Result<NativeSurface, String> {
    // Create a standalone window via winit
    let event_loop = winit::event_loop::EventLoop::new().unwrap();
    let window = winit::window::WindowBuilder::new()
        .with_title(format!("Vo Canvas: {}", id))
        .with_inner_size(winit::dpi::PhysicalSize::new(width, height))
        .build(&event_loop)
        .map_err(|e| format!("window creation failed: {}", e))?;

    // Create wgpu surface from this window
    // ...
}
```

---

## 10. Desktop-Specific Features

### 10.1 Open Local File

```rust
#[tauri::command]
fn open_file(path: String) -> Result<String, String> {
    std::fs::read_to_string(&path)
        .map_err(|e| format!("Failed to read {}: {}", path, e))
}

#[tauri::command]
fn save_file(path: String, content: String) -> Result<(), String> {
    std::fs::write(&path, &content)
        .map_err(|e| format!("Failed to write {}: {}", path, e))
}
```

### 10.2 JIT Mode Toggle

The toolbar includes a JIT toggle:

```svelte
<label>
    <input type="checkbox" bind:checked={useJit} disabled={!platform.supportsJit} />
    JIT Mode
</label>
```

Passed to `compile_and_run` and `init_gui_app` as the `mode` parameter.

### 10.3 Multiple File Project

The desktop playground can open a directory and compile multi-file projects:

```rust
#[tauri::command]
fn compile_project(path: String, mode: Option<String>) -> Result<RunResult, String> {
    let output = vo_engine::compile_with_auto_install(&path)
        .map_err(|e| format!("{}", e))?;
    let run_mode = match mode.as_deref() {
        Some("jit") => vo_engine::RunMode::Jit,
        _ => vo_engine::RunMode::Vm,
    };
    // ...
}
```

---

## 11. Build & Distribution

### Development

```bash
cd apps/playground-desktop
npm install
cargo tauri dev    # Starts Vite dev server + Tauri app with hot reload
```

### Production Build

```bash
cargo tauri build  # Produces .deb/.AppImage (Linux), .dmg (macOS), .msi (Windows)
```

### CI

```yaml
# .github/workflows/build-desktop.yml
jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions-rust-lang/setup-rust-toolchain@v1
      - uses: nicegram/nicegram/nicegram-manager/.github/actions/setup-tauri@main
      - run: cd apps/playground-desktop && cargo tauri build
```

---

## 12. Testing

### Automated Tests
- **Rust unit tests**: `cargo test -p vo-playground-desktop`
  - Compile + run simple programs
  - GUI lifecycle (init → event → render)
- **Frontend tests**: Vitest for platform abstraction
  - Mock `invoke()`, verify correct IPC messages
- **E2E**: Tauri's WebDriver support
  - Open app → type code → click Run → verify output

### Manual Test Scenarios
1. Open desktop app → type `println("hello")` → Run → see "hello" in output
2. Switch to GUI mode → run counter example → click increment → see count update
3. Run wgpu triangle demo → see triangle in native canvas overlay
4. Toggle JIT mode → run benchmark → observe faster execution
5. Open local `.vo` file → compile and run → verify filesystem access
