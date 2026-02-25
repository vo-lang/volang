# Vibe Studio — Overall Design

> Date: 2026-02-24
> Status: Design
> Supersedes: 2026-02-22 native playground design (Svelte-based)

---

## 1. Vision

Vibe Studio is a lightweight IDE for Vo that is **itself a Vo application**. It imports `vogui` for its UI and `vox` for compiling/running user code. It runs on Tauri (native desktop) and in the browser (WASM), sharing the same Vo source code for both targets.

**Key difference from the 2026-02-22 native playground design**: that design kept Svelte as the frontend framework and used Tauri only as a backend. This design eliminates Svelte entirely — the IDE's UI is a vogui view tree rendered by the existing `vogui/js` DOM renderer. The WebView contains only a minimal HTML shell + the vogui JS renderer + widget plugins (CodeMirror, xterm).

### Core Properties

| Property | Description |
|----------|-------------|
| **Self-hosted UI** | IDE chrome (toolbar, file tree, editor, output, preview) is a vogui app |
| **Two-VM model** | Host VM runs the IDE; Guest VM runs user code via vox |
| **ExternalWidget** | Escape hatch for JS-native components (CodeMirror, terminal, canvas) |
| **Cross-platform** | Same Vo source → Tauri native (fast, JIT, real FS) or WASM web (browser, VFS) |
| **Dogfooding** | The IDE is the primary stress test for vogui at scale |

---

## 2. Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                     Tauri Process (Rust)                      │
│                                                              │
│  ┌───────────────────────────┐  ┌──────────────────────────┐│
│  │ Host VM (IDE)             │  │ Guest VM (user app)      ││
│  │ imports: vogui, vox       │  │ Created by vox.RunGui()  ││
│  │                           │  │ Separate VM instance     ││
│  │ State {                   │  │ Its own PENDING_RENDER   ││
│  │   Files []FileEntry       │  │                          ││
│  │   ActiveFile string       │  │                          ││
│  │   Code string             │  └──────────────────────────┘│
│  │   Output string           │                              │
│  │   GuestRender string      │                              │
│  │   GuestRunning bool       │                              │
│  │ }                         │                              │
│  └────────────┬──────────────┘                              │
│               │                                             │
│  ┌────────────▼──────────────┐                              │
│  │ vo-tauri crate            │                              │
│  │  init_ide()               │                              │
│  │  handle_ide_event(id,p)   │                              │
│  │  VoguiPlatform impl       │                              │
│  └────────────┬──────────────┘                              │
└───────────────┼──────────────────────────────────────────────┘
                │ Tauri IPC
┌───────────────▼──────────────────────────────────────────────┐
│                      WebView                                  │
│                                                              │
│  index.html (minimal shell)                                  │
│  main.ts:                                                    │
│    1. Call init_ide() → get initial render JSON              │
│    2. Render to DOM via vogui renderer                       │
│    3. On DOM event → call handle_ide_event() → re-render     │
│                                                              │
│  Widget Registry:                                            │
│    "codemirror" → CodeMirror 6 editor                        │
│    "vogui-guest" → nested vogui renderer for user app        │
│    "xterm" → terminal emulator (future)                      │
│                                                              │
│  vogui/js renderer + morphdom + styles                       │
└──────────────────────────────────────────────────────────────┘
```

### Web (WASM) Mode

Same architecture, but:
- No Tauri process — the Rust code compiles to WASM
- `init_ide()` and `handle_ide_event()` are WASM exports (like playground's `initGuiApp`/`handleGuiEvent`)
- File system is VFS (existing playground infrastructure)
- No JIT (WASM limitation)
- IDE source is embedded in the WASM binary as a const string

---

## 3. Two-VM Model

The IDE and the user's code run in **separate VM instances**. This is critical because:
- vogui has global state (`currentState`, `handlers`, etc.) — two vogui apps cannot share a VM
- User code may panic, loop forever, or corrupt state — the IDE must survive
- The guest VM can be killed and restarted without affecting the IDE

### Data Flow

```
IDE State            vox API              Guest VM
─────────           ─────────            ─────────
s.Code ──────────→ vox.CompileString()
                         │
                    vox.RunGui(module)──→ Creates VM
                         │               Runs until blocked
                         │               emitRender → JSON
                    returns JSON ◄────── PENDING_RENDER
                         │
s.GuestRender ◄──────────┘
render() includes
  ExternalWidget("vogui-guest", {tree: s.GuestRender})

User clicks in guest UI
  → JS event from guest container
  → IDE handler: onGuestEvent(handlerId, payload)
  → IDE action calls vox.SendGuiEvent(handlerId, payload)
                         │
                    Guest VM wakes ────→ processes event
                    returns new JSON ◄── re-renders
                         │
s.GuestRender ◄──────────┘
render() → DOM update
```

### Console Apps

For non-GUI user code, the IDE uses `vox.RunCapture(module)` which returns stdout as a string. The IDE displays it in its output panel.

---

## 4. ExternalWidget

A new vogui node type that delegates rendering to an external JS widget. This is the same pattern as the existing `Canvas` node, generalized.

### Vo API

```go
type ExternalWidgetOpts struct {
    ID       string
    Type     string         // "codemirror", "vogui-guest", "xterm", etc.
    Props    map[string]any // widget-specific configuration
    OnEvent  Handler        // receives events from the widget
}

func ExternalWidget(opts ExternalWidgetOpts) Node
```

### JSON Wire Format

```json
{
  "type": "ExternalWidget",
  "props": {
    "id": "editor-main",
    "widgetType": "codemirror",
    "value": "package main\n...",
    "language": "go",
    "onEvent": 5
  }
}
```

### JS Renderer

The renderer maintains a widget registry and a live instance map:

```typescript
// Registry: type name → factory
const widgetRegistry = new Map<string, WidgetFactory>();

// Live instances: widget ID → instance
const widgetInstances = new Map<string, WidgetInstance>();

interface WidgetFactory {
  create(container: HTMLElement, props: any, onEvent: EventCallback): WidgetInstance;
}

interface WidgetInstance {
  element: HTMLElement;
  update(props: any): void;
  destroy(): void;
}
```

Renderer case:

```typescript
case 'ExternalWidget': {
  const id = node.props?.id;
  const type = node.props?.widgetType;
  let instance = widgetInstances.get(id);

  if (!instance) {
    const factory = widgetRegistry.get(type);
    const container = document.createElement('div');
    container.className = `vo-widget vo-widget-${type}`;
    container.id = `vo-widget-${id}`;
    instance = factory.create(container, node.props, (payload) => {
      handlers.handleInput(node.props?.onEvent, payload);
    });
    widgetInstances.set(id, instance);
    return instance.element;
  }

  instance.update(node.props);
  return instance.element;
}
```

### morphdom Integration

ExternalWidget elements must be preserved across re-renders (same as Canvas):

```typescript
onBeforeElUpdated(fromEl, toEl) {
  if (fromEl.classList.contains('vo-widget') && toEl.classList.contains('vo-widget')) {
    // Update props, preserve DOM element
    const id = fromEl.id.replace('vo-widget-', '');
    const instance = widgetInstances.get(id);
    if (instance) {
      instance.update(/* new props from toEl's data attributes */);
    }
    return false; // Do NOT replace the element
  }
}
```

### Widget Cleanup

When a widget's DOM element is removed (e.g., panel hidden), `onNodeRemoved` calls `instance.destroy()` and removes from `widgetInstances`.

---

## 5. Widget Implementations

### 5.1 CodeMirror Widget ("codemirror")

Props:
- `value: string` — editor content
- `language: string` — syntax highlight mode ("go" for Vo)
- `readOnly: bool` — disable editing
- `lineNumbers: bool` — show line numbers (default true)

Events:
- `onEvent` receives `{"type": "change", "value": "new content..."}` on edit

Implementation: CodeMirror 6 with `@codemirror/lang-go` for syntax highlighting. The widget creates a CodeMirror `EditorView` in its container. On content change, it fires the event callback. On `update(props)`, it sets content only if it differs (to avoid cursor reset).

### 5.2 Guest App Widget ("vogui-guest")

Props:
- `tree: string` — guest app's render JSON (the full tree, not just diff)
- `interactive: bool` — whether events are enabled

Events:
- `onEvent` receives `{"handlerId": N, "payload": "..."}` when user interacts with guest UI

Implementation: Creates a scoped container, parses `tree` JSON, renders using the same `renderNode()` function with its own morphdom root. Events from the guest DOM call the widget's event callback instead of the IDE's global handler.

### 5.3 Terminal Widget ("xterm") — Future

Props:
- `content: string` — terminal output
- `scrollToBottom: bool`

Lightweight alternative: initially, the IDE can use a `Pre` node for console output. The xterm widget can be added later for ANSI color support and interactive input.

---

## 6. vox API Extensions

Current vox API supports `Run(module)` and `RunCapture(module)`. For the IDE, we need:

```go
// RunGui starts a GUI app, returns initial render JSON.
// The module's VM stays alive for subsequent events.
func RunGui(m Module) (string, error)

// SendGuiEvent sends an event to a running GUI app.
// Returns the new render JSON after the event is processed.
func SendGuiEvent(m Module, handlerId int, payload string) (string, error)

// StopGui stops a running GUI app, signals its thread to exit, and releases its VM.
func StopGui(m Module)

// CompileCheck compiles source and returns diagnostics without running.
func CompileCheck(code string) ([]Diagnostic, error)

// Diagnostic represents a compile error with location.
type Diagnostic struct {
    File    string
    Line    int
    Column  int
    Message string
}
```

### Rust Implementation

`ExternFn = fn(&mut ExternCallContext) -> ExternResult` is a raw function pointer — it cannot capture per-guest state. The correct approach is **thread-per-guest-VM**: each guest VM runs on a dedicated OS thread, so `PENDING_RENDER` and `PENDING_HANDLER` (which are `thread_local!`) are naturally isolated with no changes to the extern registry.

`vox.RunGui` spawns a guest thread, runs the module until the fiber blocks on the event channel, then returns the initial render JSON to the caller via a `mpsc::SyncSender`. `SendGuiEvent` sends an event to the guest thread's channel and waits for the new render JSON. `StopGui` drops the sender, causing the guest thread's `recv()` to return `Err` and the thread to exit cleanly.

The `GuestHandle` (holding the event/render channels) is stored inside the opaque `Module` handle.

---

## 7. vogui Platform Layer

The existing `vogui/rust` has `#[cfg(wasm32)]` JS imports and `#[cfg(not(wasm32))]` no-op stubs. For Tauri, the native stubs need real implementations.

### VoguiPlatform Trait (from 2026-02-22 design, retained)

```rust
pub trait VoguiPlatform: Send + Sync + 'static {
    fn start_timeout(&self, id: i32, ms: i32);
    fn clear_timeout(&self, id: i32);
    fn start_interval(&self, id: i32, ms: i32);
    fn clear_interval(&self, id: i32);
    fn navigate(&self, path: &str);
    fn get_current_path(&self) -> String;
}
```

Three implementations:
- `WasmPlatform` — delegates to JS via wasm_bindgen (existing code, restructured)
- `TauriPlatform` — emits Tauri events to the webview, which runs JS timers
- `NoopPlatform` — current native stubs (for headless/test)

The active platform is set at startup via `vogui::set_platform(Box<dyn VoguiPlatform>)`.

---

## 8. IDE Vo Source Structure

```
studio/
├── main.vo          # Entry point: gui.Run(App{Init: initState, View: view})
├── state.vo         # State struct, FileEntry, Panel enum
├── views/
│   ├── layout.vo    # Top-level layout: toolbar + main area
│   ├── toolbar.vo   # Run/Stop/Reset buttons, file name display
│   ├── filetree.vo  # File list with click-to-open
│   ├── editor.vo    # ExternalWidget("codemirror", ...)
│   ├── preview.vo   # ExternalWidget("vogui-guest", ...) for GUI apps
│   └── output.vo    # Console output panel (Pre or xterm)
├── actions/
│   ├── file.vo      # openFile, saveFile, newFile
│   ├── run.vo       # runCode, stopCode, handleGuestEvent
│   └── edit.vo      # setCode (from editor onChange)
└── vo.mod
```

### Example: IDE Entry Point

```go
package main

import "vogui"
import "vox"

func main() {
    vogui.Run(vogui.App{
        Init: initState,
        View: view,
    })
}

func initState() any {
    return &State{
        Files:      listDefaultFiles(),
        ActiveFile: "main.vo",
        Code:       defaultCode,
        Panel:      PanelEditor,
    }
}

func view(state any) vogui.Node {
    s := state.(*State)
    return vogui.Column(
        toolbar(s),
        vogui.Row(
            fileTree(s).W(220),
            editorPanel(s).Flex(1),
            previewOrOutput(s).W(400),
        ).Flex(1),
    )
}
```

---

## 9. Bootstrap Sequence

### Native (Tauri)

1. Tauri process starts
2. Rust backend compiles IDE source (`studio/*.vo`) using `vo-engine`
3. Creates Host VM, registers vogui + vox externs
4. Sets `TauriPlatform` as the active vogui platform
5. Runs Host VM → `vogui.Run()` → initial render → blocks on event channel
6. Reads `PENDING_RENDER` → sends initial JSON to WebView via Tauri IPC
7. WebView's `main.ts` receives JSON → renders DOM via vogui renderer
8. User interacts → JS sends event to Rust via Tauri command → Rust sends to Host VM channel → VM processes event → re-renders → new JSON returned → JS morphs DOM

### Web (WASM)

1. Browser loads `index.html`
2. WASM binary loads (contains IDE source as embedded string + Vo runtime)
3. `init_ide()` WASM export: compiles IDE source, creates Host VM, runs until blocked
4. Returns initial render JSON to JS
5. Same render + event cycle as playground, but the app IS the IDE

---

## 10. File System

### Native (Tauri)

Real OS filesystem via vox externs. The IDE's file actions call:
- `os.ReadFile(path)` / `os.WriteFile(path, data, perm)`
- `os.ReadDir(path)` for file tree
- `filepath.Ext(path)` for file type detection

These are already implemented in vo-stdlib for native builds.

> **Scope note**: Real filesystem access (open folder, save file, project browse) is post-Phase 1. Phase 1 uses embedded example files in the IDE binary. The IDE's file actions are identical regardless — the FS layer is just not exposed to a real directory until the file-open UI is added.

### Web (WASM)

Virtual filesystem (existing VFS from playground). The IDE's file actions call the same `os.ReadFile`/`os.WriteFile` which are backed by OPFS in the WASM runtime.

No code change needed in the IDE — the Vo stdlib's platform layer handles it.

---

## 11. Compared to 2026-02-22 Design

| Aspect | 2026-02-22 Design | This Design |
|--------|-------------------|-------------|
| Frontend framework | Svelte | vogui (Vo) |
| IDE chrome | Svelte components | vogui view functions |
| Editor | Svelte `<Editor>` component | ExternalWidget("codemirror") |
| Platform abstraction | TypeScript interface | Rust VoguiPlatform trait |
| Shared code | Svelte source (web + Tauri) | Vo source (web + Tauri) |
| Build system | npm + Vite + wasm-pack | Cargo + minimal npm (widgets only) |
| Complexity | Svelte + TS + Rust + Vo | Vo + Rust + minimal TS |
| Dogfooding | Low (Svelte does the heavy lifting) | High (vogui is the framework) |

### What's Retained from 2026-02-22

- `libs/vo-tauri` crate concept (commands, gui_state, platform)
- `VoguiPlatform` trait design
- vogui/js renderer in the WebView (unchanged)
- Canvas registry (unchanged)

### What's Changed

- No Svelte — the HTML page is a minimal shell
- No `platform.ts` / `wasm-platform.ts` / `tauri-platform.ts` — platform abstraction is in Rust
- IDE source is Vo, not Svelte
- ExternalWidget is the new core mechanism for JS-native components

---

## 12. Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| vogui re-render performance with large editor content | Slow typing | ExternalWidget isolates CodeMirror from morphdom; only widget props are diffed, not editor DOM |
| Two VMs memory overhead | High memory on WASM | Guest VM is created on Run, destroyed on Stop; not persistent |
| CodeMirror bundle size in WASM mode | Large initial load | CodeMirror JS is loaded from CDN, not bundled in WASM |
| vogui lacks some IDE-specific components (split panes, tree view) | Incomplete UI | Build them in Vo as the need arises; vogui is the framework, extending it is the point |
| Guest app timer/interval conflicts with IDE | Timer ID collision | Guest VM uses a separate VoguiPlatform instance with namespaced timer IDs |
