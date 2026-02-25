# Vibe Studio — Phase 1 Development

> Date: 2026-02-24
> Parent: `2026_02_24_vibe_studio_design.md`
> Goal: End-to-end working IDE — edit code, run it, see output/GUI preview

---

## 1. Deliverable

A working Tauri desktop app where:
1. The IDE UI is rendered by vogui (a Vo application)
2. A CodeMirror editor (via ExternalWidget) lets you edit Vo code
3. Clicking "Run" compiles and executes the code via vox
4. Console output appears in an output panel
5. GUI apps render in a preview panel (via guest vogui renderer)
6. A file list lets you switch between example files

The same IDE also runs in the browser (WASM mode) with VFS instead of real filesystem.

---

## 2. Implementation Order

| Step | What | Where | Depends On |
|------|------|-------|-----------|
| S1 | ExternalWidget node type in vogui | `libs/vogui/` | — |
| S2 | ExternalWidget rendering in JS | `libs/vogui/js/` | S1 |
| S3 | VoguiPlatform trait refactor | `libs/vogui/rust/` | — |
| S4 | vox GUI API (RunGui, SendGuiEvent, StopGui) | `libs/vox/` | — |
| S5 | vo-tauri crate | `libs/vo-tauri/` | S3 |
| S6 | IDE Vo source code | `studio/` | S1, S4 |
| S7 | WebView shell + widget plugins | `studio/src/` | S2 |
| S8 | Tauri app wiring | `studio/src-tauri/` | S5, S6, S7 |
| S9 | WASM mode (web) | `studio/wasm/` | S6, S7 |
| S10 | CodeMirror widget | `studio/src/widgets/` | S2 |
| S11 | Guest app widget | `studio/src/widgets/` | S2, S4 |

S1-S4 are independent and can be done in parallel.
S5-S11 depend on the foundations.

---

## 3. Step Details

### S1: ExternalWidget Node Type (vogui Vo)

**New file: `libs/vogui/widget.vo`**

```go
package vogui

// ExternalWidgetOpts configures an external JS widget.
type ExternalWidgetOpts struct {
    ID       string         // Unique widget instance ID (required)
    Type     string         // Widget type name (registered in JS)
    Props    map[string]any // Widget-specific configuration
    OnEvent  Handler        // Receives events from the widget
}

// ExternalWidget creates a node that delegates rendering to a JS widget.
// The JS renderer looks up the widget type in its registry and creates
// or updates the widget instance.
func ExternalWidget(opts ExternalWidgetOpts) Node {
    props := map[string]any{
        "id":         opts.ID,
        "widgetType": opts.Type,
    }
    // Merge user props
    for k, v := range opts.Props {
        props[k] = v
    }
    if opts.OnEvent.ID >= 0 {
        props["onEvent"] = opts.OnEvent.ID
    }
    return Node{Type: "ExternalWidget", Props: props}
}
```

Also add `OnWidget` handler to `event.vo` (or `widget.vo`):

```go
// OnWidget registers a handler for ExternalWidget events.
// The action receives the raw payload string sent by the widget implementation.
// Action signature: func(s *State, payload string)
func OnWidget(action any) Handler { return registerHandler(action, handlerWidget, 0) }
```

And in `invokeHandler` in `event.vo`, add the dispatch case:

```go
case handlerWidget:
    fn := handler.action.(func(any, string))
    fn(state, event.Payload)  // raw payload, no JSON unwrapping
```

No changes to `app.vo` or other existing files.

---

### S2: ExternalWidget JS Rendering

**Modified: `libs/vogui/js/src/renderer.ts`**

Add widget registry infrastructure and the `ExternalWidget` case:

```typescript
// ── Widget Registry ──

export interface WidgetFactory {
  create(container: HTMLElement, props: any, onEvent: (payload: string) => void): WidgetInstance;
}

export interface WidgetInstance {
  element: HTMLElement;
  update(props: any): void;
  destroy(): void;
}

const widgetRegistry = new Map<string, WidgetFactory>();
const widgetInstances = new Map<string, WidgetInstance>();

export function registerWidget(type: string, factory: WidgetFactory) {
  widgetRegistry.set(type, factory);
}

export function destroyAllWidgets() {
  widgetInstances.forEach(inst => inst.destroy());
  widgetInstances.clear();
}
```

Renderer case (inside the `switch (node.type)` block):

```typescript
case 'ExternalWidget': {
  const id = node.props?.id ?? 'widget-0';
  const widgetType = node.props?.widgetType ?? '';
  const domId = `vo-widget-${id}`;

  // Check if instance already exists (morphdom preserved the element)
  let instance = widgetInstances.get(id);
  if (instance) {
    instance.update(node.props);
    return instance.element;
  }

  // Create new instance
  const factory = widgetRegistry.get(widgetType);
  if (!factory) {
    const el = document.createElement('div');
    el.className = 'vo-widget vo-widget-missing';
    el.textContent = `Unknown widget: ${widgetType}`;
    return el;
  }

  // Widget implementations send raw string payloads.
  // We route directly via config.onEvent (no handleInput wrapping) so
  // the Vo side uses OnWidget which receives the raw string.
  const onEvent = (rawPayload: string) => {
    if (interactive && config.onEvent && node.props?.onEvent !== undefined) {
      config.onEvent(node.props.onEvent, rawPayload);
    }
  };

  instance = factory.create(
    document.createElement('div'),
    node.props,
    onEvent
  );
  instance.element.className = `vo-widget vo-widget-${widgetType}`;
  instance.element.id = domId;
  instance.element.dataset.widgetId = id;
  widgetInstances.set(id, instance);
  return instance.element;
}
```

**morphdom hooks** (in the `render()` function that calls morphdom):

```typescript
morphdom(target, newTree, {
  onBeforeElUpdated(fromEl, toEl) {
    // Preserve ExternalWidget elements
    if (fromEl.dataset?.widgetId && toEl.dataset?.widgetId
        && fromEl.dataset.widgetId === toEl.dataset.widgetId) {
      const instance = widgetInstances.get(fromEl.dataset.widgetId);
      if (instance) {
        // Extract new props from toEl (stored as data attributes or parsed from the new tree)
        // For now, the update happens in the renderNode case above
        // since morphdom calls renderNode for the new tree first
      }
      return false; // Do NOT replace
    }
    // ... existing hooks (canvas, focus preservation, etc.)
  },
  onNodeDiscarded(node: Node) {
    if (node instanceof HTMLElement && node.dataset?.widgetId) {
      const instance = widgetInstances.get(node.dataset.widgetId);
      if (instance) {
        instance.destroy();
        widgetInstances.delete(node.dataset.widgetId);
      }
    }
  }
});
```

---

### S3: VoguiPlatform Trait Refactor

**Modified: `libs/vogui/rust/src/lib.rs`**

Replace `#[cfg]` blocks with trait dispatch:

```rust
use std::sync::OnceLock;

pub trait VoguiPlatform: Send + Sync + 'static {
    fn start_timeout(&self, id: i32, ms: i32);
    fn clear_timeout(&self, id: i32);
    fn start_interval(&self, id: i32, ms: i32);
    fn clear_interval(&self, id: i32);
    fn navigate(&self, path: &str);
    fn get_current_path(&self) -> String;
}

static PLATFORM: OnceLock<Box<dyn VoguiPlatform>> = OnceLock::new();

pub fn set_platform(platform: Box<dyn VoguiPlatform>) {
    let _ = PLATFORM.set(platform);
}

pub fn platform() -> &'static dyn VoguiPlatform {
    PLATFORM.get().map(|b| b.as_ref()).unwrap_or(&NoopPlatform)
}
```

> **Phase 1 limitation**: The singleton platform means all VMs (host and guest) share the same timer namespace. In Phase 1 this is acceptable because guest apps that use `timer.SetTimeout` will fire events through the host platform — timer IDs may collide if both host and guest use timers simultaneously. Guest VMs in Phase 1 demos should be simple (no long-running timers). Timer ID namespacing (e.g., `(vm_id << 32) | timer_id`) is deferred to post-Phase 1.

```rust

struct NoopPlatform;
impl VoguiPlatform for NoopPlatform {
    fn start_timeout(&self, _id: i32, _ms: i32) {}
    fn clear_timeout(&self, _id: i32) {}
    fn start_interval(&self, _id: i32, _ms: i32) {}
    fn clear_interval(&self, _id: i32) {}
    fn navigate(&self, _path: &str) {}
    fn get_current_path(&self) -> String { "/".to_string() }
}
```

**Modified: `libs/vogui/rust/src/externs.rs`**

Timer/navigation externs dispatch through `crate::platform()`:

```rust
#[vo_fn("vogui", "startTimeout")]
pub fn start_timeout(ctx: &mut ExternCallContext) -> ExternResult {
    let id = ctx.arg_i64(slots::ARG_ID) as i32;
    let ms = ctx.arg_i64(slots::ARG_MS) as i32;
    crate::platform().start_timeout(id, ms);
    ExternResult::Ok
}
// ... same pattern for all timer/navigation externs
```

**WASM platform**: Extract current `wasm_bindgen` imports into `WasmPlatform` struct implementing the trait. Set it during WASM init.

**Native stubs**: Removed — replaced by `NoopPlatform` (the default).

---

### S4: vox GUI API

**Modified: `libs/vox/vox.vo`** — add new functions:

```go
// RunGui starts a GUI app and returns the initial render JSON.
// The module's VM stays alive for subsequent SendGuiEvent calls.
func RunGui(m Module) (string, error)

// SendGuiEvent sends an event to a running GUI app.
// Returns the new render JSON.
func SendGuiEvent(m Module, handlerId int, payload string) (string, error)

// StopGui stops a running GUI app, signals its thread to exit, and releases its VM.
func StopGui(m Module)

// CompileCheck compiles source and returns error diagnostics without running.
func CompileCheck(code string) ([]Diagnostic, error)

type Diagnostic struct {
    File    string
    Line    int
    Column  int
    Message string
}
```

**New: `libs/vox/rust/src/gui.rs`** — Rust implementation:

The key challenge: running a guest vogui app inside the host's process without global state collision.

**Approach: Thread-per-guest-VM**

`ExternFn = fn(&mut ExternCallContext) -> ExternResult` is a raw function pointer — it cannot capture per-guest state. The solution is to run each guest VM on a **dedicated OS thread**. Because `PENDING_RENDER` and `PENDING_HANDLER` are thread-local (`thread_local!`), each guest thread has its own isolated copies. No changes to the extern registry or `ExternFn` signature are needed.

```rust
use std::sync::{mpsc, Arc, Mutex};

pub struct GuestHandle {
    /// Channel to send events to the guest thread.
    event_tx: mpsc::SyncSender<(i32, String)>,
    /// Channel to receive new render JSON from the guest thread.
    render_rx: mpsc::Receiver<Result<String, String>>,
}

pub fn run_gui(module: vo_engine::Module) -> Result<(String, GuestHandle), String> {
    let (event_tx, event_rx) = mpsc::sync_channel::<(i32, String)>(0);
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<String, String>>(1);

    std::thread::spawn(move || {
        // Each thread has its own PENDING_RENDER / PENDING_HANDLER TLS.
        // Standard vogui externs (emitRender, registerEventHandler) work unchanged.
        let mut vm = create_vm_from_module(&module);
        register_vogui_externs(&mut vm);
        vm.run_until_blocked();
        let json = vogui_rust::take_pending_render().unwrap_or_default();
        let _ = render_tx.send(Ok(json));
        // Event loop
        while let Ok((handler_id, payload)) = event_rx.recv() {
            invoke_event_handler(&mut vm, handler_id, &payload);
            vm.run_until_blocked();
            let json = vogui_rust::take_pending_render().unwrap_or_default();
            let _ = render_tx.send(Ok(json));
        }
        // event_rx closed → exit thread
    });

    let initial_json = render_rx.recv().map_err(|e| e.to_string())??;
    let handle = GuestHandle { event_tx, render_rx };
    Ok((initial_json, handle))
}

pub fn send_gui_event(handle: &GuestHandle, handler_id: i32, payload: String) -> Result<String, String> {
    handle.event_tx.send((handler_id, payload)).map_err(|e| e.to_string())?;
    handle.render_rx.recv().map_err(|e| e.to_string())?
}

pub fn stop_gui(handle: GuestHandle) {
    // Dropping event_tx closes the channel; guest thread exits its while-let loop.
    drop(handle.event_tx);
}
```

The `GuestHandle` is stored in the `Module` handle (via a wrapper type with opaque `any` in Vo). `vox.StopGui` drops the handle, signaling the thread to exit.

---

### S5: vo-tauri Crate

**Retained from 2026-02-22 design** with modifications. The crate now manages the **IDE's host VM**, not user code directly (the host VM manages user code via vox).

**`libs/vo-tauri/src/lib.rs`**:

```rust
pub struct VoTauriState {
    pub host: Mutex<HostVmState>,
}

pub struct HostVmState {
    vm: vo_engine::Vm,
    event_handler: GcRef,  // The IDE's sendEvent closure
}
```

**`libs/vo-tauri/src/commands.rs`**:

```rust
#[tauri::command]
fn init_ide(state: tauri::State<VoTauriState>) -> Result<String, String> {
    // 1. Compile IDE source (embedded or from disk)
    // 2. Create host VM
    // 3. Set TauriPlatform as vogui platform
    // 4. Register vogui + vox externs
    // 5. Run until blocked → take PENDING_RENDER
    // 6. Store VM in state
    // 7. Return initial render JSON
}

#[tauri::command]
fn handle_ide_event(
    state: tauri::State<VoTauriState>,
    handler_id: i32,
    payload: String,
) -> Result<String, String> {
    // 1. Get host VM from state
    // 2. Invoke the event handler closure with (handler_id, payload)
    // 3. Run VM until blocked → take PENDING_RENDER
    // 4. Return new render JSON
}
```

**`libs/vo-tauri/src/platform.rs`**:

```rust
pub struct TauriPlatform {
    app_handle: tauri::AppHandle,
}

impl VoguiPlatform for TauriPlatform {
    fn start_timeout(&self, id: i32, ms: i32) {
        // Emit event to webview: webview JS starts setTimeout
        let _ = self.app_handle.emit("vo-start-timeout", (id, ms));
    }
    fn start_interval(&self, id: i32, ms: i32) {
        let _ = self.app_handle.emit("vo-start-interval", (id, ms));
    }
    // ... etc
}
```

The WebView JS listens for these events and manages timers, calling `handle_ide_event` when timers fire (same pattern as playground's `vo.ts`).

---

### S6: IDE Vo Source Code

**Minimal IDE for Phase 1**:

```
studio/app/
├── main.vo
├── state.vo
├── view.vo
├── actions.vo
└── vo.mod
```

**`state.vo`**:

```go
package main

type Panel int

const (
    PanelEditor Panel = iota
    PanelOutput
    PanelPreview
)

type FileEntry struct {
    Name    string
    Content string
}

type State struct {
    // File management
    Files      []FileEntry
    ActiveIdx  int

    // Editor
    Code string

    // Execution
    Output       string
    IsRunning    bool
    IsGuiApp     bool
    GuestRender  string
    GuestModule  vox.Module // nil when not running
    CompileError string

    // UI
    RightPanel Panel
}
```

**`view.vo`**:

```go
package main

import "vogui"

func view(state any) vogui.Node {
    s := state.(*State)
    return vogui.Column(
        toolbar(s),
        vogui.Row(
            fileList(s).W(200),
            editor(s).Flex(1),
            rightPanel(s).W(400),
        ).Flex(1),
    )
}

func toolbar(s *State) vogui.Node {
    return vogui.Row(
        vogui.Button("Run", vogui.On(runCode)).Bg("#22c55e").Fg("#fff"),
        vogui.Button("Stop", vogui.On(stopCode)).Bg("#ef4444").Fg("#fff"),
        vogui.Text(activeFileName(s)).Fg("#888"),
        vogui.Spacer(),
        vogui.Text("Vibe Studio").Bold().Fg("#666"),
    ).P(8).Gap(8).Bg("#1e1e2e")
}

func fileList(s *State) vogui.Node {
    var items []any
    for _, f := range s.Files {
        items = append(items, f)
    }
    return vogui.Column(
        vogui.Text("Files").Bold().Font(12).Fg("#888"),
        vogui.ForEach(items, func(item any, i int) vogui.Node {
            f := item.(FileEntry)
            active := i == s.ActiveIdx
            bg := "#transparent"
            if active {
                bg = "#333"
            }
            return vogui.Text(f.Name).
                P(6).Bg(bg).Fg("#ccc").Cursor("pointer").
                W("100%").
                On(vogui.OnInt(openFile, i))
        }),
    ).Bg("#181825").P(8).Gap(4)
}

func editor(s *State) vogui.Node {
    return vogui.ExternalWidget(vogui.ExternalWidgetOpts{
        ID:   "main-editor",
        Type: "codemirror",
        Props: map[string]any{
            "value":    s.Code,
            "language": "go",
        },
        OnEvent: vogui.OnWidget(onEditorChange),
    })
}

func rightPanel(s *State) vogui.Node {
    if s.IsGuiApp && s.GuestRender != "" {
        return vogui.ExternalWidget(vogui.ExternalWidgetOpts{
            ID:   "guest-preview",
            Type: "vogui-guest",
            Props: map[string]any{
                "tree":        s.GuestRender,
                "interactive": true,
            },
            OnEvent: vogui.OnWidget(onGuestEvent),
        })
    }
    // Console output
    return vogui.Column(
        vogui.Text("Output").Bold().Font(12).Fg("#888"),
        vogui.Pre(s.Output).Flex(1).Fg("#ccc").Bg("#11111b").P(8),
    ).Bg("#181825")
}
```

**`actions.vo`**:

```go
package main

import "libs/vox"

func onEditorChange(s *State, value string) {
    s.Code = value
}

func runCode(s *State) {
    s.Output = ""
    s.CompileError = ""
    s.GuestRender = ""
    s.IsGuiApp = false

    m, err := vox.CompileString(s.Code)
    if err != nil {
        s.CompileError = err.Error()
        s.Output = "Compile error: " + err.Error()
        return
    }

    // Detect GUI app
    isGui := containsGuiImport(s.Code)
    if isGui {
        json, err := vox.RunGui(m)
        if err != nil {
            s.Output = "Runtime error: " + err.Error()
            return
        }
        s.IsGuiApp = true
        s.GuestRender = json
        s.GuestModule = m
        s.RightPanel = PanelPreview
    } else {
        output, err := vox.RunCapture(m)
        if err != nil {
            s.Output = "Runtime error: " + err.Error()
        } else {
            s.Output = output
        }
        s.RightPanel = PanelOutput
    }
}

func stopCode(s *State) {
    if s.IsGuiApp && s.GuestModule != nil {
        vox.StopGui(s.GuestModule)
        s.GuestModule = nil
    }
    s.IsGuiApp = false
    s.GuestRender = ""
    s.IsRunning = false
}

func onGuestEvent(s *State, payload string) {
    // payload is raw JSON: {"handlerId": N, "payload": "..."}
    var evt struct {
        HandlerID int    `json:"handlerId"`
        Payload   string `json:"payload"`
    }
    if err := json.Unmarshal([]byte(payload), &evt); err != nil {
        s.Output = "Guest event parse error: " + err.Error()
        return
    }
    newRender, err := vox.SendGuiEvent(s.GuestModule, evt.HandlerID, evt.Payload)
    if err != nil {
        s.Output = "Guest runtime error: " + err.Error()
        s.IsGuiApp = false
        return
    }
    s.GuestRender = newRender
}

func containsGuiImport(code string) bool {
    // Simple string check for `import "vogui"`
    return strings.Contains(code, `"vogui"`)
}
```

---

### S7: WebView Shell

**`studio/src/index.html`**:

```html
<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8" />
  <title>Vibe Studio</title>
  <link rel="stylesheet" href="./styles/vogui.css" />
  <link rel="stylesheet" href="./styles/studio.css" />
</head>
<body>
  <div id="app"></div>
  <script type="module" src="./main.ts"></script>
</body>
</html>
```

**`studio/src/main.ts`**:

```typescript
import { render, registerWidget, destroyAllWidgets } from '@vogui/renderer';
import { codemirrorWidget } from './widgets/codemirror';
import { voguiGuestWidget } from './widgets/vogui-guest';
import './styles/vogui.css';

// Register widget plugins
registerWidget('codemirror', codemirrorWidget);
registerWidget('vogui-guest', voguiGuestWidget);

// Platform detection: Tauri or WASM
const isTauri = '__TAURI__' in window;

let handleEvent: (id: number, payload: string) => Promise<string>;

async function init() {
  let initialJson: string;

  if (isTauri) {
    const { invoke } = await import('@tauri-apps/api/core');
    initialJson = await invoke('init_ide');
    handleEvent = (id, payload) => invoke('handle_ide_event', { handlerId: id, payload });
  } else {
    // WASM mode
    const wasm = await import('./wasm/studio.js');
    await wasm.default();
    initialJson = wasm.init_ide();
    handleEvent = async (id, payload) => wasm.handle_ide_event(id, payload);
  }

  // Initial render
  const app = document.getElementById('app')!;
  render(app, JSON.parse(initialJson).tree, {
    interactive: true,
    onEvent: async (handlerId: number, payload: string) => {
      const newJson = await handleEvent(handlerId, payload);
      const parsed = JSON.parse(newJson);
      render(app, parsed.tree, { interactive: true, onEvent: /* same */ });
    },
  });
}

init();
```

Note: The `render()` function from vogui/js wraps `renderNode` + morphdom. This may need a small addition to vogui/js to export a top-level `render(target, tree, config)` function.

---

### S8: Tauri App Wiring

**`studio/src-tauri/Cargo.toml`**:

```toml
[package]
name = "vibe-studio"
version = "0.1.0"
edition = "2021"

[dependencies]
tauri = { version = "2", features = ["protocol-asset"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
vo-tauri = { path = "../../libs/vo-tauri" }

[build-dependencies]
tauri-build = { version = "2" }
```

**`studio/src-tauri/src/main.rs`**:

```rust
fn main() {
    let builder = tauri::Builder::default();
    let builder = vo_tauri::register_commands(builder);
    builder
        .run(tauri::generate_context!())
        .expect("error running Vibe Studio");
}
```

**`studio/src-tauri/tauri.conf.json`** — standard Tauri config pointing to `../src` as the frontend directory.

---

### S10: CodeMirror Widget

**`studio/src/widgets/codemirror.ts`**:

```typescript
import { EditorView, basicSetup } from 'codemirror';
import { EditorState } from '@codemirror/state';
import { go } from '@codemirror/lang-go';
import { oneDark } from '@codemirror/theme-one-dark';
import type { WidgetFactory, WidgetInstance } from '@vogui/renderer';

export const codemirrorWidget: WidgetFactory = {
  create(container, props, onEvent) {
    const state = EditorState.create({
      doc: props.value ?? '',
      extensions: [
        basicSetup,
        go(),
        oneDark,
        EditorView.updateListener.of((update) => {
          if (update.docChanged) {
            // Send raw content string; Vo side uses OnWidget which receives it verbatim.
            onEvent(update.state.doc.toString());
          }
        }),
      ],
    });
    const view = new EditorView({ state, parent: container });

    return {
      element: container,
      update(newProps) {
        const currentDoc = view.state.doc.toString();
        if (newProps.value !== undefined && newProps.value !== currentDoc) {
          view.dispatch({
            changes: { from: 0, to: currentDoc.length, insert: newProps.value },
          });
        }
      },
      destroy() {
        view.destroy();
      },
    } satisfies WidgetInstance;
  },
};
```

---

### S11: Guest App Widget

**`studio/src/widgets/vogui-guest.ts`**:

```typescript
import { renderNode } from '@vogui/renderer';
import morphdom from 'morphdom';
import type { WidgetFactory, WidgetInstance } from '@vogui/renderer';

export const voguiGuestWidget: WidgetFactory = {
  create(container, props, onEvent) {
    const root = document.createElement('div');
    root.className = 'vogui-guest-root';
    container.appendChild(root);

    function renderTree(treeJson: string) {
      if (!treeJson) return;
      try {
        const parsed = JSON.parse(treeJson);
        const tree = parsed.tree ?? parsed;
        const newDom = renderNode(tree, {
          interactive: props.interactive ?? true,
          onEvent: (handlerId: number, payload: string) => {
            // Route guest events back to IDE via the widget's onEvent
            onEvent(JSON.stringify({ handlerId, payload }));
          },
        });
        if (newDom) {
          if (root.firstChild) {
            morphdom(root.firstChild, newDom);
          } else {
            root.appendChild(newDom);
          }
        }
      } catch (e) {
        root.textContent = 'Guest render error: ' + e;
      }
    }

    renderTree(props.tree);

    return {
      element: container,
      update(newProps) {
        if (newProps.tree) {
          renderTree(newProps.tree);
        }
      },
      destroy() {
        root.innerHTML = '';
      },
    } satisfies WidgetInstance;
  },
};
```

---

## 4. Directory Layout (Final)

```
studio/
├── app/                          # IDE Vo source
│   ├── main.vo
│   ├── state.vo
│   ├── view.vo
│   ├── actions.vo
│   └── vo.mod
├── src/                          # WebView frontend (minimal)
│   ├── index.html
│   ├── main.ts                   # Bootstrap: init + render loop
│   ├── styles/
│   │   ├── vogui.css             # Symlink to libs/vogui/js/src/styles.ts output
│   │   └── studio.css            # IDE-specific styles (dark theme)
│   └── widgets/
│       ├── codemirror.ts         # CodeMirror 6 widget
│       └── vogui-guest.ts        # Nested vogui renderer widget
├── src-tauri/                    # Tauri Rust backend
│   ├── Cargo.toml
│   ├── tauri.conf.json
│   ├── build.rs
│   └── src/
│       └── main.rs
├── wasm/                         # WASM entry point (for web mode)
│   ├── Cargo.toml
│   └── src/lib.rs                # init_ide() + handle_ide_event() WASM exports
├── package.json                  # npm: codemirror, morphdom, @tauri-apps/api
└── vite.config.ts                # Vite for bundling frontend
```

Changes in existing code:

```
libs/vogui/
├── widget.vo                     # NEW: ExternalWidget node type
├── js/src/renderer.ts            # MODIFIED: ExternalWidget case + widget registry
└── rust/src/
    ├── lib.rs                    # MODIFIED: VoguiPlatform trait + set_platform()
    └── externs.rs                # MODIFIED: dispatch through platform()

libs/vox/
├── vox.vo                        # MODIFIED: add RunGui, SendGuiEvent, StopGui, CompileCheck
└── rust/src/
    ├── gui.rs                    # NEW: GuestGuiState, run_gui, send_gui_event
    └── ffi.rs                    # MODIFIED: register new FFI functions

libs/vo-tauri/                    # NEW: Tauri bridge crate
├── Cargo.toml
└── src/
    ├── lib.rs
    ├── commands.rs
    ├── gui_state.rs
    └── platform.rs
```

---

## 5. Build & Run

### Native (Tauri)

```bash
cd studio
npm install                        # CodeMirror, morphdom, etc.
cargo tauri dev                    # Builds Rust + serves frontend
```

The Tauri dev server:
1. Builds `src-tauri/` (links vo-tauri, vo-engine, vox, vogui/rust)
2. Starts Vite dev server for `src/`
3. Opens a native window with the WebView

### Web (WASM)

```bash
cd studio/wasm
wasm-pack build --target web       # Builds WASM binary with embedded IDE source
cd ..
npm run dev                        # Vite serves src/ + WASM
```

---

## 6. Milestone Criteria (Phase 1 Done)

- [ ] ExternalWidget renders in vogui (unit test: widget appears in DOM)
- [ ] VoguiPlatform trait works (WASM + Tauri + Noop)
- [ ] `vox.RunGui` / `SendGuiEvent` work (test: guest tetris renders + responds to events)
- [ ] Tauri app launches, shows IDE UI rendered by vogui
- [ ] CodeMirror editor is usable (type code, syntax highlight, content flows to State)
- [ ] Click "Run" on console app → output appears in panel
- [ ] Click "Run" on GUI app → guest vogui renders in preview panel
- [ ] Guest app events work (click buttons in preview → guest app responds)
- [ ] File list lets you switch between 3+ example files
- [ ] Web WASM mode works (same UI in browser, no Tauri)

---

## 7. What's NOT in Phase 1

These will be added iteratively as the IDE matures:

- Real filesystem integration (Tauri FS API) — Phase 1 uses embedded example files
- LSP / diagnostics overlay in editor
- Terminal widget (xterm)
- Split pane resizing (fixed widths in Phase 1)
- Hot reload
- Debug mode / breakpoints
- Settings / preferences
- Project management (open folder, vo.mod, multi-file compilation)
- Mobile responsive layout
