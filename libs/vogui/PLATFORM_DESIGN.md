# VoGUI Platform Design Document

> vogui as a platform SDK — like iOS UIKit for the Vo ecosystem.
> Third parties build libraries and apps on top of it.

## 1. Vision

vogui is the **application runtime SDK** for Vo. It provides:

1. **UI primitives** — layout, text, input, overlay, navigation (already done)
2. **Canvas primitive** — GPU-capable `<canvas>` element for 2D/3D rendering (new)
3. **Event system** — unified input routing for both DOM widgets and canvas (enhance)
4. **Extension canvas access** — third-party WASM libraries can bind to canvas elements (new)
5. **Application lifecycle** — state management, routing, timers (already done)

### Analogy

| iOS UIKit | vogui |
|-----------|-------|
| UIView | Node |
| UIButton, UILabel | Button, Text |
| UICanvas / CALayer | Canvas |
| UIKit event system | vogui event system |
| Third-party frameworks (SpriteKit, SceneKit) | Third-party Vo libs (canvas2d, wgpu-bindings, etc.) |
| App Store apps | Vo apps using `import "vogui"` + third-party libs |

### What vogui does NOT do

- vogui does NOT implement rendering engines (WebGL, wgpu, physics, etc.)
- vogui does NOT dictate what third parties draw on a canvas
- vogui provides the **canvas element and event routing**; third parties provide the **content**

---

## 2. Current Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Vo App Code                            │
│            State struct + view() + action functions          │
└───────────────────────────┬─────────────────────────────────┘
                            │ calls vogui API
┌───────────────────────────▼─────────────────────────────────┐
│                    vogui (Vo Library)                        │
│   15 .vo files: node, layout, display, input, event,        │
│   control, container, overlay, form, list, nav, app,        │
│   timer, router, util                                       │
│                                                             │
│   8 extern functions:                                       │
│     registerEventHandler, emitRender,                       │
│     startTimeout, clearTimeout, startInterval, clearInterval│
│     navigate, getCurrentPath                                │
└───────────────────────────┬─────────────────────────────────┘
                            │ JSON via stdout (__VOGUI__ prefix)
┌───────────────────────────▼─────────────────────────────────┐
│              vogui Rust (libs/vogui/rust/)                   │
│   Extern implementations: event handler registration,       │
│   render output, timer forwarding, navigation               │
│   JS imports via wasm_bindgen (WASM) or stubs (native)      │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│              vogui JS (libs/vogui/js/)                       │
│   renderer.ts: renderNode() — 60+ node types → DOM elements│
│   morphdom for efficient DOM updates                        │
│   styles.ts: CSS for all vo-* classes                       │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│              Playground (playground/)                        │
│   Svelte app: Editor, Output, GuiPreview, FileExplorer      │
│   vo.ts: WASM loading, timer globals, ext module loading    │
│   rust/src/lib.rs: VM lifecycle, GUI state management       │
│                                                             │
│   Extension Bridge (vo-web/runtime-wasm/ext_bridge.rs):     │
│   voSetupExtModule(key, bytes) → WebAssembly.instantiate    │
│   voCallExt(name, input) → output (bytes-in/bytes-out)      │
└─────────────────────────────────────────────────────────────┘
```

### Current Limitations

| Issue | Impact |
|-------|--------|
| No canvas/GPU node type | Cannot render 2D/3D graphics |
| Extension ABI is stateless bytes-in/bytes-out | Extensions cannot hold WebGL context or persistent GPU state |
| Extensions loaded as raw WASM (no DOM access) | Extensions cannot call WebGL/WebGPU APIs |
| Render output via stdout `__VOGUI__` prefix | Fragile, single-channel, mixes with print output |
| No canvas input events (mouse, touch, pointer) | Cannot build interactive canvas applications |
| No Fullscreen API | Cannot go fullscreen for immersive apps |

---

## 3. Target Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Vo App Code                            │
│  import "vogui"                                             │
│  import "github.com/someone/physics2d"   ← third-party lib │
│                                                             │
│  func view(state any) vogui.Node {                          │
│      return vogui.Column(                                   │
│          vogui.Text("Physics Simulation"),                   │
│          vogui.Canvas(vogui.CanvasOpts{                     │
│              ID: "sim", Width: 800, Height: 600,            │
│              OnPointer: vogui.OnPointer(handlePointer),     │
│          }),                                                │
│          vogui.Button("Reset", vogui.On(reset)),            │
│      )                                                      │
│  }                                                          │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│                    vogui (Vo Library)                        │
│   Existing: layout, display, input, event, etc.             │
│   New: canvas.vo — Canvas() component + canvas events       │
└───────────────────────────┬─────────────────────────────────┘
                            │ JSON tree (Canvas node included)
┌───────────────────────────▼─────────────────────────────────┐
│              vogui JS Runtime                                │
│   Existing: renderNode() for all DOM node types             │
│   New: Canvas node handler                                  │
│     1. Create/reuse <canvas> element                        │
│     2. Register in voCanvasRegistry                         │
│     3. Attach pointer/mouse/touch/resize listeners          │
│     4. Handle fullscreen transitions                        │
│     5. On destroy: unregister + cleanup                     │
└───────────────────────────┬─────────────────────────────────┘
                            │
┌───────────────────────────▼─────────────────────────────────┐
│           Canvas Registry (Platform Service)                 │
│   voCanvasRegistry: Map<string, HTMLCanvasElement>           │
│   voGetCanvas(id) → HTMLCanvasElement | null                │
│   voRegisterCanvas(id, el) / voUnregisterCanvas(id)         │
└───────────────────────────┬─────────────────────────────────┘
                            │ third-party ext accesses canvas
┌───────────────────────────▼─────────────────────────────────┐
│        Third-Party Extension WASM (wasm-bindgen)            │
│   e.g. github.com/someone/physics2d                         │
│                                                             │
│   Rust code:                                                │
│     let canvas = voGetCanvas("sim");  // via web_sys        │
│     let ctx = canvas.get_context("webgl2");                 │
│     // ... render using WebGL/wgpu/whatever                 │
│                                                             │
│   Ships: .wasm + .js (wasm-bindgen output)                  │
└─────────────────────────────────────────────────────────────┘
```

---

## 4. Canvas Node Design

### 4.1 Vo API (`canvas.vo`)

```go
package vogui

// CanvasOpts configures a Canvas element.
type CanvasOpts struct {
    ID         string // Unique canvas identifier (required)
    Width      int    // Canvas width in pixels
    Height     int    // Canvas height in pixels
    Fullscreen bool   // Whether canvas is in fullscreen mode
    OnPointer  Handler // Pointer events (mouse + touch unified)
    OnResize   Handler // Canvas resize events
}

// Canvas creates a GPU-capable canvas element.
// Third-party libraries access this canvas via its ID.
func Canvas(opts CanvasOpts) Node {
    props := map[string]any{
        "id":     opts.ID,
        "width":  opts.Width,
        "height": opts.Height,
    }
    if opts.Fullscreen {
        props["fullscreen"] = true
    }
    if opts.OnPointer.ID >= 0 {
        props["onPointer"] = opts.OnPointer.ID
    }
    if opts.OnResize.ID >= 0 {
        props["onResize"] = opts.OnResize.ID
    }
    return Node{Type: "Canvas", Props: props}
}
```

### 4.2 Event Handlers for Canvas

New handler types needed:

```go
// PointerEvent contains unified mouse/touch event data.
type PointerEvent struct {
    Kind    string  // "down", "up", "move", "enter", "leave"
    X       float64 // Canvas-relative X
    Y       float64 // Canvas-relative Y
    Button  int     // 0=left, 1=middle, 2=right (mouse only)
    Buttons int     // Bitmask of pressed buttons
}

// OnPointer wraps an action that receives pointer events.
// Action signature: func(s *State, e PointerEvent)
func OnPointer(action any) Handler {
    return registerHandler(action, handlerPointer, 0)
}

// OnResize wraps an action that receives canvas resize.
// Action signature: func(s *State, width int, height int)
func OnResize(action any) Handler {
    return registerHandler(action, handlerResize, 0)
}
```

### 4.3 Fullscreen Control

```go
// RequestFullscreen puts a canvas into fullscreen mode.
// In the next render cycle, the canvas node should have Fullscreen: true.
// Fullscreen exit is triggered by the browser (Esc key) or by setting Fullscreen: false.
```

Fullscreen is a **state-driven prop** on the Canvas node, not an imperative call.
The JS runtime calls `canvas.requestFullscreen()` when the prop changes from false → true,
and `document.exitFullscreen()` when true → false. The browser's Esc key triggers a
`fullscreenchange` event, which the JS runtime routes back to Vo as a resize event with
the new dimensions.

**Browser constraint**: `requestFullscreen()` must be called from a user gesture (click).
The JS runtime queues the fullscreen request and executes it on the next user-initiated event.

### 4.4 JSON Wire Format

```json
{
  "type": "Canvas",
  "props": {
    "id": "my-canvas",
    "width": 800,
    "height": 600,
    "fullscreen": false,
    "onPointer": 5,
    "onResize": 6
  }
}
```

---

## 5. JS Runtime: Canvas Node Handling

### 5.1 Canvas Rendering in `renderer.ts`

```typescript
case 'Canvas': {
    const id = node.props?.id ?? 'vo-canvas-0';
    const width = node.props?.width ?? 300;
    const height = node.props?.height ?? 150;

    const el = document.createElement('canvas');
    el.id = `vo-canvas-${id}`;
    el.className = 'vo-canvas';
    el.width = width;
    el.height = height;
    if (style) el.style.cssText = style;

    // Register in platform canvas registry
    (window as any).voRegisterCanvas(id, el);

    // Pointer events (unified mouse + touch)
    if (interactive && node.props?.onPointer !== undefined) {
        const ptrHandler = node.props.onPointer;
        for (const evtType of ['pointerdown', 'pointerup', 'pointermove', 'pointerenter', 'pointerleave']) {
            el.addEventListener(evtType, (e: PointerEvent) => {
                const rect = el.getBoundingClientRect();
                const payload = JSON.stringify({
                    kind: evtType.replace('pointer', ''),
                    x: e.clientX - rect.left,
                    y: e.clientY - rect.top,
                    button: e.button,
                    buttons: e.buttons,
                });
                config.onEvent?.(ptrHandler, payload);
            });
        }
    }

    // Fullscreen
    if (node.props?.fullscreen) {
        // Queue for next user gesture
        el.addEventListener('click', () => {
            if (!document.fullscreenElement) {
                el.requestFullscreen();
            }
        }, { once: true });
    }

    return el;
}
```

### 5.2 Canvas Lifecycle in morphdom

When morphdom updates the DOM:
- **Canvas element persists** (morphdom preserves elements with same tag+id)
- **Canvas context is NOT lost** on re-render (critical for GPU state)
- `onBeforeElUpdated` must skip canvas content updates — only update event handlers and dimensions
- On element removal: call `voUnregisterCanvas(id)` to clean up

```typescript
onBeforeElUpdated(fromEl, toEl) {
    // Preserve canvas GPU state — never replace canvas elements
    if (fromEl.tagName === 'CANVAS' && toEl.tagName === 'CANVAS') {
        // Update dimensions if changed
        if (fromEl.width !== toEl.width) fromEl.width = toEl.width;
        if (fromEl.height !== toEl.height) fromEl.height = toEl.height;
        // Copy event handlers
        // ... (existing handler copy logic)
        return false; // Do NOT replace the element
    }
    // ... existing logic
}
```

### 5.3 Canvas Registry (`vo.ts`)

```typescript
const voCanvasRegistry = new Map<string, HTMLCanvasElement>();

(window as any).voGetCanvas = (id: string): HTMLCanvasElement | null => {
    return voCanvasRegistry.get(id) ?? null;
};

(window as any).voRegisterCanvas = (id: string, canvas: HTMLCanvasElement) => {
    voCanvasRegistry.set(id, canvas);
};

(window as any).voUnregisterCanvas = (id: string) => {
    voCanvasRegistry.delete(id);
};
```

---

## 6. Extension ABI Evolution

### 6.1 Current: Standalone ABI (keep as-is for pure compute)

```
Extension ships:  module.wasm (standalone, no JS glue)
Loading:          WebAssembly.instantiate(bytes)
Calling:          ext.exports[name](input_ptr, input_len, out_len_ptr) → output_ptr
Use cases:        resvg (SVG→PNG), image processing, data transforms
```

### 6.2 New: wasm-bindgen ABI (for canvas/DOM-accessing extensions)

```
Extension ships:  module_bg.wasm + module.js (wasm-bindgen output)
Loading:          import(module.js).then(m => m.default(module_bg.wasm))
Calling:          module.ext_function_name(input) → output
Canvas access:    Extension calls window.voGetCanvas(id) via web_sys
Use cases:        2D canvas drawing, WebGL rendering, wgpu, physics + rendering
```

### 6.3 Module Manifest Evolution

Current `vo.ext.toml`:
```toml
[extension]
name = "vox"
path = "../../target/{profile}/libvo_vox"
```

Enhanced for WASM:
```toml
[extension]
name = "canvas2d"

[extension.wasm]
type = "bindgen"          # "standalone" (default) or "bindgen"
wasm = "canvas2d_bg.wasm"
js   = "canvas2d.js"      # wasm-bindgen JS glue (only for type = "bindgen")
```

### 6.4 Enhanced Module Loading in `vo.ts`

```typescript
(window as any).voSetupExtModule = async (
    key: string,
    wasmBytes: Uint8Array,
    jsGlueUrl?: string
) => {
    if (jsGlueUrl) {
        // wasm-bindgen module: full DOM access
        const glue = await import(/* @vite-ignore */ jsGlueUrl);
        await glue.default(wasmBytes);
        extBindgenModules.set(key, glue);
    } else {
        // Standalone module: current behavior
        const { instance } = await WebAssembly.instantiate(wasmBytes);
        extInstances.set(key, instance);
    }
};
```

### 6.5 Extension Calling Convention for Canvas Extensions

For wasm-bindgen extensions, `voCallExt` dispatches differently:

```typescript
(window as any).voCallExt = (externName: string, input: Uint8Array): Uint8Array => {
    // Try bindgen modules first
    for (const [key, mod] of extBindgenModules) {
        if (externName.startsWith(key)) {
            const funcName = externName.replace(key + '_', '');
            if (typeof mod[funcName] === 'function') {
                return mod[funcName](input);
            }
        }
    }
    // Fall back to standalone modules (current behavior)
    // ...
};
```

---

## 7. Third-Party Library Architecture

### 7.1 Library Types

| Type | Ships | Canvas | Example |
|------|-------|--------|---------|
| **Pure Vo** | `.vo` files only | No | UI component library |
| **Compute** | `.vo` + standalone `.wasm` | No | resvg, data processing |
| **Canvas** | `.vo` + bindgen `.wasm` + `.js` | Yes | 2D canvas, WebGL renderer, physics sim |

### 7.2 Example: Third-Party Canvas Library

A third-party creates `github.com/someone/draw2d`:

**Vo API** (`draw2d.vo`):
```go
package draw2d

import "vogui"

// Scene holds drawing state (opaque, managed by Rust extension).
type Scene any

// NewScene creates a new drawing scene bound to a canvas ID.
func NewScene(canvasID string) (Scene, error)

// Clear clears the scene.
func Clear(s Scene)

// Rect draws a rectangle.
func Rect(s Scene, x, y, w, h float64, color string)

// Circle draws a circle.
func Circle(s Scene, cx, cy, r float64, color string)

// Flush renders all pending draw commands to the canvas.
func Flush(s Scene)

// Free releases the scene resources.
func Free(s Scene)
```

**Rust implementation** (`draw2d/rust/src/lib.rs`):
```rust
use wasm_bindgen::prelude::*;
use web_sys::{window, HtmlCanvasElement, CanvasRenderingContext2d};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = window, js_name = "voGetCanvas")]
    fn vo_get_canvas(id: &str) -> Option<HtmlCanvasElement>;
}

// Extension creates a 2D context from the platform-provided canvas,
// then draws using standard Canvas2D API.
```

**App using it**:
```go
package main

import "vogui"
import "github.com/someone/draw2d"

type State struct {
    scene draw2d.Scene
}

func view(state any) vogui.Node {
    s := state.(*State)
    // Draw scene
    draw2d.Clear(s.scene)
    draw2d.Rect(s.scene, 10, 10, 100, 50, "#ff0000")
    draw2d.Circle(s.scene, 200, 200, 50, "#00ff00")
    draw2d.Flush(s.scene)

    return vogui.Column(
        vogui.H2("Draw2D Demo"),
        vogui.Canvas(vogui.CanvasOpts{
            ID: "main", Width: 800, Height: 600,
            OnPointer: vogui.OnPointer(handlePointer),
        }),
        vogui.Button("Clear", vogui.On(clear)),
    )
}
```

---

## 8. Render Output Channel Cleanup

### Current Problem

Render output goes through stdout with `__VOGUI__` prefix:
```rust
vo_runtime::output::write("__VOGUI__");
vo_runtime::output::writeln(json);
```

This is fragile: user `println` calls in the same program mix with render output.

### Solution

Replace stdout-based render output with a dedicated extern channel.

**Vo side** (`app.vo`): No change to `emitRender(json string)` extern signature.

**Rust side** (`externs.rs`): Instead of writing to stdout, store render JSON in a
thread-local that the playground reads directly:

```rust
thread_local! {
    pub static PENDING_RENDER: RefCell<Option<String>> = RefCell::new(None);
}

#[vo_fn("vogui", "emitRender")]
pub fn emit_render(ctx: &mut ExternCallContext) -> ExternResult {
    let json_ref = ctx.arg_ref(slots::ARG_JSON);
    let json = if json_ref.is_null() { String::new() } else { string::as_str(json_ref).to_string() };
    PENDING_RENDER.with(|s| *s.borrow_mut() = Some(json));
    ExternResult::Ok
}
```

**Playground side** (`rust/src/lib.rs`): Read `PENDING_RENDER` instead of parsing stdout.

This also means user `println` output goes to the console panel cleanly, without
interfering with GUI rendering.

---

## 9. Development Plan

### Phase 1: Canvas Node Foundation

**Goal**: Canvas element appears in vogui tree, platform manages its lifecycle.

| Task | Location | Description |
|------|----------|-------------|
| 1.1 | `libs/vogui/canvas.vo` | `Canvas(CanvasOpts) Node` + `CanvasOpts` type |
| 1.2 | `libs/vogui/js/src/renderer.ts` | `case 'Canvas'` in `renderNode()` |
| 1.3 | `libs/vogui/js/src/renderer.ts` | morphdom `onBeforeElUpdated` preserves canvas elements |
| 1.4 | `libs/vogui/js/src/styles.ts` | `.vo-canvas` CSS class |
| 1.5 | `playground/src/wasm/vo.ts` | `voCanvasRegistry` + `voGetCanvas` + `voRegisterCanvas` + `voUnregisterCanvas` |

**Deliverable**: A vogui app can include `vogui.Canvas(...)` and see a canvas element in the DOM.

### Phase 2: Canvas Events

**Goal**: Pointer events on canvas are routed through vogui's event system.

| Task | Location | Description |
|------|----------|-------------|
| 2.1 | `libs/vogui/canvas.vo` | `PointerEvent` type, `OnPointer(action)` handler |
| 2.2 | `libs/vogui/event.vo` | `handlerPointer` constant + `invokeHandler` case |
| 2.3 | `libs/vogui/js/src/renderer.ts` | Pointer event listeners on canvas element |
| 2.4 | `libs/vogui/canvas.vo` | `OnResize(action)` handler |
| 2.5 | `libs/vogui/event.vo` | `handlerResize` constant + `invokeHandler` case |

**Deliverable**: Vo code receives mouse/touch events from canvas.

### Phase 3: Fullscreen

**Goal**: Canvas can enter/exit fullscreen mode.

| Task | Location | Description |
|------|----------|-------------|
| 3.1 | `libs/vogui/js/src/renderer.ts` | `requestFullscreen` / `exitFullscreen` on prop change |
| 3.2 | `libs/vogui/js/src/renderer.ts` | `fullscreenchange` event → resize event back to Vo |
| 3.3 | `playground/src/pages/Playground.svelte` | Fullscreen-aware layout (hide editor when canvas fullscreen) |

**Deliverable**: `CanvasOpts{Fullscreen: true}` makes the canvas go fullscreen.

### Phase 4: wasm-bindgen Extension ABI

**Goal**: Third-party extensions compiled with wasm-bindgen can access canvas via DOM.

| Task | Location | Description |
|------|----------|-------------|
| 4.1 | `playground/src/wasm/vo.ts` | Enhanced `voSetupExtModule` with JS glue support |
| 4.2 | `playground/src/wasm/vo.ts` | Enhanced `voCallExt` dispatch for bindgen modules |
| 4.3 | `vo-web/runtime-wasm/ext_bridge.rs` | `load_wasm_ext_module` support for bindgen type |
| 4.4 | `vo-module/` | Fetch `.js` glue alongside `.wasm` from GitHub releases |
| 4.5 | Documentation | Extension author guide for canvas extensions |

**Deliverable**: A third-party wasm-bindgen extension can call `voGetCanvas(id)` and get
a live `HTMLCanvasElement` to render into.

### Phase 5: Render Channel Cleanup

**Goal**: Eliminate stdout-based render output.

| Task | Location | Description |
|------|----------|-------------|
| 5.1 | `libs/vogui/rust/src/externs.rs` | `PENDING_RENDER` thread-local instead of stdout |
| 5.2 | `libs/vogui/rust/src/lib.rs` | `take_pending_render()` public API |
| 5.3 | `playground/rust/src/lib.rs` | Read `PENDING_RENDER` instead of parsing stdout |
| 5.4 | `playground/rust/src/lib.rs` | Clean up `extract_render_json` (remove `__VOGUI__` parsing) |

**Deliverable**: Render JSON and println output are cleanly separated.

### Phase 6: Reference GPU Extension (wgpu)

**Goal**: Ship a first-party wgpu extension as a reference implementation for GPU canvas rendering.

Canvas is a **platform component** provided by vogui (Phase 1-3). Third-party extensions
like wgpu access the canvas via `voGetCanvas(id)` and render into it using WebGPU.

| Task | Location | Description |
|------|----------|-------------|
| 6.1 | `3rdparty/wgpu/rust/` | wasm-bindgen crate: `voGetCanvas(id)` → WebGPU device + pipeline |
| 6.2 | `3rdparty/wgpu/wgpu.vo` | Vo API: `Init(canvasID)`, `Clear(r,g,b)`, `DrawTriangle(...)`, `Present()` |
| 6.3 | `3rdparty/wgpu/` | Build `.wasm` + `.js` glue via `wasm-pack build --target web` |
| 6.4 | `playground/src/assets/examples/` | wgpu demo: colored triangle on a vogui Canvas |
| 6.5 | `github.com/vo-lang/wgpu` | Push release (`.vo`, `.wasm`, `.js`, `vo.mod`, `vo.ext.toml`) |
| 6.6 | Documentation | Tutorial: "Building a GPU Extension for Vo" |

**Deliverable**: A working wgpu extension that renders a triangle on a vogui Canvas,
demonstrating the full platform → extension → GPU pipeline.

---

## 10. Open Questions

### Q1: Canvas rendering timing

When does the third-party extension render to the canvas?

**Option A**: Extension renders during `voCallExt(render_fn, state_data)`.
The Vo library calls its render extern during `view()`, before the JSON tree is emitted.
The canvas already exists in the DOM from the previous render cycle.

**Option B**: Extension has its own `requestAnimationFrame` loop.
The Vo library initializes the extension once; it then renders continuously.
State updates are pushed to the extension via a separate channel.

**Recommendation**: Option A for event-driven apps, Option B for continuous animation.
Both can coexist. vogui's `SetInterval(16, tickFn)` gives ~60fps for Option A.

### Q2: Multiple canvases

Should an app support multiple canvas elements simultaneously?

**Answer**: Yes. Each canvas has a unique `ID`. The canvas registry is keyed by ID.
Different third-party extensions can render to different canvases in the same app.

### Q3: Canvas inside Modal/Drawer

A canvas inside a `Modal(open=false)` should not be rendered/registered.
When the modal opens, the canvas is created and registered.
When it closes, the canvas is destroyed and unregistered.

**Implementation**: morphdom `onNodeRemoved` callback calls `voUnregisterCanvas`.

### Q4: Native (non-WASM) canvas support

For native desktop builds (future), the canvas concept maps to a native window/surface.
The extension ABI would use a different backend (raw OpenGL, Vulkan, Metal).
This is out of scope for now but the Vo API (`Canvas(CanvasOpts)`) should be
platform-agnostic — the same Vo code works on web and native.

---

## Appendix A: Building a GPU Extension for Vo

This guide walks through creating a wasm-bindgen extension that renders into a vogui
Canvas. The reference implementation is `github.com/vo-lang/wgpu`.

### Repository structure

```
your-extension/
├── wgpu.vo          # Vo API (package declaration + extern functions)
├── vo.mod           # module github.com/your-org/your-ext
├── vo.ext.toml      # [extension] metadata
├── wgpu.wasm        # Built wasm-bindgen binary
├── wgpu.js          # Built wasm-bindgen JS glue
└── rust/            # Rust source
    ├── Cargo.toml
    ├── .cargo/config.toml   # --cfg=web_sys_unstable_apis for WebGPU
    └── src/lib.rs
```

### Step 1: Rust crate

Create a `cdylib` crate with `wasm-bindgen` + `web-sys`:

```toml
[lib]
crate-type = ["cdylib"]

[dependencies]
wasm-bindgen = "0.2"
wasm-bindgen-futures = "0.4"
js-sys = "0.3"
web-sys = { version = "0.3", features = ["Window", "HtmlCanvasElement", "Gpu", ...] }
```

For WebGPU APIs, add `.cargo/config.toml`:

```toml
[build]
target = "wasm32-unknown-unknown"
rustflags = ["--cfg=web_sys_unstable_apis"]
```

### Step 2: Access the canvas

vogui registers every `Canvas` element in a global registry. Your extension accesses
it via `voGetCanvas(id)`:

```rust
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = window, js_name = "voGetCanvas")]
    fn vo_get_canvas(id: &str) -> Option<HtmlCanvasElement>;
}
```

### Step 3: Async initialization

Use `#[wasm_bindgen(start)]` for async setup (GPU adapter, device, etc.).
This runs during `voSetupExtModule` before any Vo code calls your extension:

```rust
#[wasm_bindgen(start)]
pub async fn start() -> Result<(), JsValue> {
    // Request GPU adapter + device here
    // Store in thread_local! for later use
    Ok(())
}
```

### Step 4: Export functions

Export synchronous functions that Vo calls via `voCallExt`. Each function receives
a `&[u8]` input (JSON-encoded arguments) and returns `Vec<u8>` (JSON response):

```rust
#[wasm_bindgen(js_name = "Init")]
pub fn init(input: &[u8]) -> Vec<u8> { ... }

#[wasm_bindgen(js_name = "Render")]
pub fn render(input: &[u8]) -> Vec<u8> { ... }
```

### Step 5: Vo API wrapper

Declare extern functions matching the wasm-bindgen export names, plus high-level
wrappers that serialize arguments to JSON:

```vo
package wgpu

// High-level API for users
func InitCanvas(canvasID string) error {
    input := "{\"canvas_id\":\"" + canvasID + "\"}"
    result, err := Init(input)
    if err != nil { return err }
    return checkResult(result)
}

// Extern — name must match wasm-bindgen export
func Init(input string) ([]byte, error)
```

### Step 6: Build and publish

```bash
cd rust/
RUSTFLAGS="--cfg=web_sys_unstable_apis" wasm-pack build --target web --release --out-dir ../pkg
cp pkg/vo_wgpu_bg.wasm ../wgpu.wasm
cp pkg/vo_wgpu.js ../wgpu.js
```

Push to GitHub with a version tag (e.g. `v0.1.0`). The playground's module
fetcher will download `wgpu.vo`, `wgpu.wasm`, and `wgpu.js` automatically.

### Step 7: Use in Vo

```vo
import "vogui"
import "github.com/vo-lang/wgpu@v0.1.0"

func Init() vogui.Node {
    return vogui.Canvas(vogui.CanvasOpts{ID: "gpu", Width: 640, Height: 480})
}
// After canvas is in DOM:
wgpu.InitCanvas("gpu")
wgpu.RenderFrame(0.1, 0.2, 0.3, 1.0, true)  // clear + triangle
```

### Architecture notes

- **Canvas is a platform component** — vogui owns it, extensions borrow it
- **Each extension is independent** — different extensions can use different GPU stacks
- **Extensions share only the canvas element** via `voGetCanvas(id)`
- **Vo API is cross-platform** — same `.vo` code works on web and (future) native
