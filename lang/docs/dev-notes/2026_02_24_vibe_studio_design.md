# Vibe Studio — Overall Design

> Date: 2026-02-24
> Status: Design (revised)
> Supersedes: vogui-based design (same date, earlier version)

---

## 1. Vision

Vibe Studio is a lightweight IDE for Vo with a **Svelte-native frontend**. It uses Tauri for the native desktop target and compiles to WASM for the browser target. The Vo runtime (`vox`) is embedded in the Rust backend. The IDE chrome (toolbar, file tree, editor, output, preview) is built entirely in Svelte — no vogui is used for the IDE UI itself.

User code that uses `vogui` still works — the guest app's render JSON is displayed via the existing `vogui/js` renderer embedded in a Svelte component.

### Core Properties

| Property | Description |
|----------|-------------|
| **Svelte frontend** | IDE chrome is a Svelte app; all state via Svelte stores |
| **No Host VM** | IDE is native TypeScript/Svelte, no Vo VM for the IDE itself |
| **Guest VM** | User code runs in a Rust-managed VM (thread-per-guest); vox API |
| **Bridge abstraction** | `bridge.ts` abstracts Tauri IPC vs WASM exports with identical API |
| **Cross-platform** | Same Svelte source → Tauri native (fast, JIT, real FS) or WASM web (browser, VFS) |
| **Simplicity** | No ExternalWidget, no VoguiPlatform trait, no Host VM lifecycle |

---

## 2. Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                     Tauri Process (Rust)                      │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │ Tauri Commands                                        │   │
│  │  compile_run(code) → stdout string                   │   │
│  │  run_gui(code)     → initial render JSON             │   │
│  │  send_gui_event(handler_id, payload) → render JSON   │   │
│  │  stop_gui()                                          │   │
│  │  compile_check(code) → []Diagnostic                  │   │
│  └────────────────────────────┬─────────────────────────┘   │
│                               │                              │
│  ┌────────────────────────────▼─────────────────────────┐   │
│  │ Guest VM (user code)                                  │   │
│  │  Created on run_gui(); thread-per-guest isolation     │   │
│  │  PENDING_RENDER / PENDING_HANDLER are thread-local    │   │
│  └──────────────────────────────────────────────────────┘   │
└───────────────────────────────┬──────────────────────────────┘
                                │ Tauri IPC
┌───────────────────────────────▼──────────────────────────────┐
│                      WebView — Svelte App                     │
│                                                              │
│  App.svelte                                                  │
│   ├── Toolbar.svelte     (run/stop buttons, file name)       │
│   ├── FileTree.svelte    (file list, click-to-switch)        │
│   ├── Editor.svelte      (CodeMirror 6, native component)    │
│   ├── OutputPanel.svelte (pre-formatted console output)      │
│   └── PreviewPanel.svelte (renders guest vogui JSON)         │
│                                                              │
│  stores/ide.ts           (writable Svelte store)             │
│  lib/bridge.ts           (Tauri | WASM abstraction)          │
└──────────────────────────────────────────────────────────────┘
```

### Web (WASM) Mode

Same Svelte frontend, but `bridge.ts` detects the absence of `__TAURI__` and switches to calling WASM exports:

```
studio/wasm/src/lib.rs  →  wasm-pack  →  studio.js + studio_bg.wasm
```

WASM exports mirror the Tauri command set: `compile_run`, `run_gui`, `send_gui_event`, `stop_gui`, `compile_check`. The guest VM runs synchronously on the WASM thread (no separate OS thread).

---

## 3. Guest VM Model

User code runs in a Rust-managed VM. The IDE itself has no VM — it is a Svelte app.

### Data Flow

```
Svelte Store          bridge.ts (Tauri/WASM)   Guest VM (Rust)
────────────          ──────────────────────   ───────────────
code ─────────────→  run_gui(code)
                           │ compile + start VM
                           │ run until blocked
                     ◄─────┘ initial render JSON
guestRender ◄────────┘

User clicks in PreviewPanel
  → PreviewPanel calls bridge.send_gui_event(handlerId, payload)
                           │
                     ◄─────┘ new render JSON
guestRender ◄────────┘
PreviewPanel re-renders via vogui/js renderNode()
```

### Console Apps

`bridge.compile_run(code)` compiles and runs the module, capturing stdout. The return value is the full captured output string. No persistent VM state after the call.

---

## 4. State Management

All IDE state lives in a Svelte writable store:

```typescript
// stores/ide.ts
export interface IdeState {
  files: FileEntry[];
  activeIdx: number;
  code: string;
  output: string;
  isRunning: boolean;
  isGuiApp: boolean;
  guestRender: string;   // JSON string from guest VM
  compileError: string;
}

export interface FileEntry {
  name: string;
  content: string;
}

export const ide = writable<IdeState>({ /* initial */ });
```

Svelte components subscribe to `ide` and update it via actions in `lib/actions.ts`.

---

## 5. Bridge Abstraction

`lib/bridge.ts` provides a uniform API regardless of backend:

```typescript
export interface Bridge {
  compileRun(code: string): Promise<string>;       // returns stdout
  runGui(code: string): Promise<string>;           // returns initial render JSON
  sendGuiEvent(handlerId: number, payload: string): Promise<string>;
  stopGui(): Promise<void>;
  compileCheck(code: string): Promise<Diagnostic[]>;
}

export interface Diagnostic {
  file: string;
  line: number;
  column: number;
  message: string;
}

export async function createBridge(): Promise<Bridge> {
  if ('__TAURI__' in window) {
    const { invoke } = await import('@tauri-apps/api/core');
    return {
      compileRun:    (code)               => invoke('compile_run', { code }),
      runGui:        (code)               => invoke('run_gui', { code }),
      sendGuiEvent:  (handlerId, payload) => invoke('send_gui_event', { handlerId, payload }),
      stopGui:       ()                   => invoke('stop_gui'),
      compileCheck:  (code)               => invoke('compile_check', { code }),
    };
  } else {
    const wasm = await import('./wasm/studio.js');
    await wasm.default();
    return {
      compileRun:    (code)               => Promise.resolve(wasm.compile_run(code)),
      runGui:        (code)               => Promise.resolve(wasm.run_gui(code)),
      sendGuiEvent:  (handlerId, payload) => Promise.resolve(wasm.send_gui_event(handlerId, payload)),
      stopGui:       ()                   => { wasm.stop_gui(); return Promise.resolve(); },
      compileCheck:  (code)               => Promise.resolve(wasm.compile_check(code)),
    };
  }
}
```

---

## 6. Guest App Rendering (PreviewPanel)

When user code uses `vogui`, the guest VM emits render JSON. `PreviewPanel.svelte` renders it using the `vogui/js` renderer directly — no ExternalWidget, no widget registry, just a direct call to `renderNode`:

```svelte
<!-- PreviewPanel.svelte -->
<script lang="ts">
  import { renderNode } from '@vogui/renderer';
  import morphdom from 'morphdom';
  import { bridge } from '../lib/bridge';

  export let guestRender: string;
  export let interactive: boolean = true;

  let root: HTMLElement;
  let currentDom: HTMLElement | null = null;

  $: if (guestRender && root) {
    renderGuest(guestRender);
  }

  function renderGuest(json: string) {
    const parsed = JSON.parse(json);
    const tree = parsed.tree ?? parsed;
    const newDom = renderNode(tree, {
      interactive,
      onEvent: async (handlerId: number, payload: string) => {
        const newJson = await bridge.sendGuiEvent(handlerId, payload);
        renderGuest(newJson);
      },
    });
    if (!newDom) return;
    if (currentDom) {
      morphdom(currentDom, newDom);
    } else {
      root.appendChild(newDom);
      currentDom = newDom as HTMLElement;
    }
  }
</script>

<div bind:this={root} class="preview-panel"></div>
```

---

## 7. Tauri Commands (Rust)

```rust
// studio/src-tauri/src/lib.rs

#[tauri::command]
async fn compile_run(code: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    // Compile + run, capture stdout
}

#[tauri::command]
async fn run_gui(code: String, state: tauri::State<'_, AppState>) -> Result<String, String> {
    // Compile + start guest VM thread, return initial render JSON
}

#[tauri::command]
async fn send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    // Forward event to guest thread, return new render JSON
}

#[tauri::command]
async fn stop_gui(state: tauri::State<'_, AppState>) -> Result<(), String> {
    // Drop guest handle, signal thread exit
}

#[tauri::command]
async fn compile_check(code: String) -> Result<Vec<Diagnostic>, String> {
    // Compile only, return diagnostics
}
```

`AppState` holds an `Option<GuestHandle>` (the mpsc channel pair to the guest thread). Since Tauri state requires `Send + Sync`, the handle is wrapped in `Mutex`.

---

## 8. vox API (Rust side)

The Rust-side vox API is identical to the vogui design — thread-per-guest isolation is still correct:

```rust
pub struct GuestHandle {
    event_tx: mpsc::SyncSender<(i32, String)>,
    render_rx: mpsc::Receiver<Result<String, String>>,
}

pub fn run_gui(code: &str) -> Result<(String, GuestHandle), String>;
pub fn send_gui_event(handle: &GuestHandle, handler_id: i32, payload: String) -> Result<String, String>;
pub fn stop_gui(handle: GuestHandle);
pub fn compile_run(code: &str) -> Result<String, String>;
pub fn compile_check(code: &str) -> Result<Vec<Diagnostic>, String>;
```

These functions live in `libs/vox/rust/src/` and are called directly from Tauri commands and WASM exports.

---

## 9. Directory Structure

```
studio/
├── src/                          # Svelte frontend
│   ├── App.svelte                # Root: layout + store subscription
│   ├── components/
│   │   ├── Toolbar.svelte        # Run/Stop buttons, file name, status
│   │   ├── FileTree.svelte       # File list with click-to-switch
│   │   ├── Editor.svelte         # CodeMirror 6 (native Svelte component)
│   │   ├── OutputPanel.svelte    # Pre-formatted console output
│   │   └── PreviewPanel.svelte   # Renders guest vogui JSON via renderNode
│   ├── stores/
│   │   └── ide.ts                # IdeState writable store
│   ├── lib/
│   │   ├── bridge.ts             # Tauri | WASM abstraction
│   │   ├── actions.ts            # runCode, stopCode, switchFile, onEditorChange
│   │   └── examples.ts           # Embedded example files
│   └── main.ts                   # Svelte mount
├── src-tauri/                    # Tauri Rust backend
│   ├── Cargo.toml
│   ├── tauri.conf.json
│   ├── build.rs
│   └── src/
│       ├── main.rs
│       └── lib.rs                # AppState + all Tauri commands
├── wasm/                         # WASM entry point (for web mode)
│   ├── Cargo.toml
│   └── src/lib.rs                # WASM exports mirroring Tauri commands
├── package.json                  # svelte, codemirror, morphdom, @tauri-apps/api
├── vite.config.ts
└── svelte.config.ts
```

Changes in existing code:

```
libs/vox/
├── vox.vo                        # MODIFIED: RunGui, SendGuiEvent, StopGui, CompileCheck
└── rust/src/
    ├── gui.rs                    # NEW: GuestHandle, run_gui, send_gui_event, stop_gui
    └── ffi.rs                    # MODIFIED: register new externs
```

No changes to `libs/vogui/` — vogui is unchanged. Its JS renderer is used by `PreviewPanel.svelte` but the IDE does not depend on vogui's Vo or Rust layers.

---

## 10. Bootstrap Sequence

### Native (Tauri)

1. Tauri process starts; `AppState { guest: Mutex<None> }` initialized
2. Browser loads Svelte app from Vite-built assets
3. `App.svelte` mounts; calls `createBridge()` → returns Tauri bridge
4. IDE loads example files into store; displays initial editor content
5. User types/edits in Editor.svelte (CodeMirror) → updates `ide.code` in store
6. User clicks Run → `actions.runCode()` → `bridge.compileRun(code)` or `bridge.runGui(code)`
7. Tauri command executes in Rust; result returned to Svelte
8. Store updated → Svelte reactivity re-renders OutputPanel or PreviewPanel

### Web (WASM)

1. Browser loads `index.html`
2. Svelte app boots; `createBridge()` detects no `__TAURI__` → loads WASM module
3. WASM module initializes (contains vox + vo-engine compiled to WASM)
4. Same render/event cycle; WASM functions called synchronously (no threading)

---

## 11. Comparison

| Aspect | vogui design (earlier) | This Design (Svelte) |
|--------|------------------------|----------------------|
| IDE frontend | vogui Vo app | Svelte components |
| IDE state | Vo State struct in Host VM | TypeScript IdeState in Svelte store |
| Host VM | Yes (runs IDE) | No |
| Editor integration | ExternalWidget + widget registry | Native CodeMirror Svelte component |
| Guest app display | ExternalWidget("vogui-guest") | Direct `renderNode()` in PreviewPanel.svelte |
| VoguiPlatform trait | Required (IDE timer/nav) | Not needed (IDE has no timers via vogui) |
| ExternalWidget in vogui | New feature required | Not needed |
| Studio Vo source | `studio/app/*.vo` | None |
| Complexity | Vo + Rust + minimal TS | Svelte + TS + Rust |
| IDE UI iteration | Recompile Vo, restart VM | Vite HMR |
| Type safety | Vo type system | TypeScript |

---

## 12. Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Guest app timer events in WASM (no threads) | Timers don't fire during event wait | WASM guest runs synchronously; timers handled by existing WasmPlatform |
| vogui/js renderer version mismatch | Guest app renders incorrectly | PreviewPanel imports from the same `@vogui/renderer` package; version is pinned |
| Tauri command blocking on guest VM event | UI freeze | Tauri commands are async; guest thread blocks independently |
| Guest VM panic corrupts AppState | IDE crash | GuestHandle is replaced on each Run; panics in guest thread do not propagate |
