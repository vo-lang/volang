# Vibe Studio — Phase 1 Development

> Date: 2026-02-24
> Parent: `2026_02_24_vibe_studio_design.md`
> Goal: End-to-end working IDE — edit code, run it, see output/GUI preview

---

## 1. Deliverable

A working Tauri desktop app where:
1. The IDE UI is a Svelte application (dark-themed, professional layout)
2. A CodeMirror 6 editor (native Svelte component) lets you edit Vo code
3. Clicking "Run" compiles and executes the code via vox (Rust backend)
4. Console output appears in an output panel
5. GUI apps render in a preview panel (vogui/js renderer in a Svelte component)
6. A file tree lets you switch between example files

The same Svelte app also runs in the browser (WASM mode) with VFS instead of real filesystem. No Host VM, no ExternalWidget system, no Vo source for the IDE itself.

---

## 2. Implementation Order

| Step | What | Where | Depends On |
|------|------|-------|-----------|
| S1 | vox Rust GUI API (run_gui, send_gui_event, stop_gui, compile_run) | `libs/vox/rust/` | — |
| S2 | Tauri commands + AppState | `studio/src-tauri/` | S1 |
| S3 | Svelte app skeleton + stores | `studio/src/` | — |
| S4 | Bridge abstraction (Tauri vs WASM) | `studio/src/lib/bridge.ts` | S2 |
| S5 | Editor.svelte (CodeMirror 6) | `studio/src/components/` | S3 |
| S6 | PreviewPanel.svelte (vogui/js guest renderer) | `studio/src/components/` | S3, S4 |
| S7 | Full IDE layout (Toolbar, FileTree, OutputPanel) | `studio/src/components/` | S3 |
| S8 | WASM mode | `studio/wasm/` | S1 |

S1, S3 are independent. S2 depends on S1. S4 depends on S2. S5-S7 depend on S3-S4. S8 depends on S1.

---

## 3. Step Details

### S1: vox Rust GUI API

**New: `libs/vox/rust/src/gui.rs`**

Thread-per-guest isolation: each guest VM runs on its own OS thread. `PENDING_RENDER` and `PENDING_HANDLER` are `thread_local!`, so each guest thread has naturally isolated copies.

```rust
use std::sync::mpsc;

pub struct GuestHandle {
    /// Send (handler_id, payload) to the guest thread.
    pub event_tx: mpsc::SyncSender<(i32, String)>,
    /// Receive new render JSON from the guest thread.
    pub render_rx: mpsc::Receiver<Result<String, String>>,
}

pub fn run_gui(code: &str) -> Result<(String, GuestHandle), String> {
    let module = compile(code)?;
    let (event_tx, event_rx) = mpsc::sync_channel::<(i32, String)>(0);
    let (render_tx, render_rx) = mpsc::sync_channel::<Result<String, String>>(1);

    std::thread::spawn(move || {
        let mut vm = create_vm_from_module(module);
        register_vogui_externs(&mut vm);
        vm.run_until_blocked();
        let json = vogui_rust::take_pending_render().unwrap_or_default();
        let _ = render_tx.send(Ok(json));
        while let Ok((handler_id, payload)) = event_rx.recv() {
            invoke_event_handler(&mut vm, handler_id, &payload);
            vm.run_until_blocked();
            let json = vogui_rust::take_pending_render().unwrap_or_default();
            let _ = render_tx.send(Ok(json));
        }
    });

    let initial_json = render_rx.recv().map_err(|e| e.to_string())??;
    Ok((initial_json, GuestHandle { event_tx, render_rx }))
}

pub fn send_gui_event(handle: &GuestHandle, handler_id: i32, payload: String) -> Result<String, String> {
    handle.event_tx.send((handler_id, payload)).map_err(|e| e.to_string())?;
    handle.render_rx.recv().map_err(|e| e.to_string())?
}

pub fn stop_gui(handle: GuestHandle) {
    drop(handle.event_tx);  // Closes channel; guest thread exits while-let
}

pub fn compile_run(code: &str) -> Result<String, String> {
    let module = compile(code)?;
    let mut vm = create_vm_from_module(module);
    vm.run_captured()
}
```

**Modified: `libs/vox/rust/src/ffi.rs`** — register `run_gui`, `send_gui_event`, `stop_gui`, `compile_run` as Tauri-callable functions (not vox Vo externs — these are called from Tauri commands directly).

---

### S2: Tauri Commands + AppState

**`studio/src-tauri/src/lib.rs`**:

```rust
use std::sync::Mutex;
use vox::gui::{GuestHandle, compile_run, run_gui, send_gui_event, stop_gui};

pub struct AppState {
    guest: Mutex<Option<GuestHandle>>,
}

#[tauri::command]
async fn cmd_compile_run(
    code: String,
    _state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    compile_run(&code)
}

#[tauri::command]
async fn cmd_run_gui(
    code: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let (initial_json, handle) = run_gui(&code)?;
    *state.guest.lock().unwrap() = Some(handle);
    Ok(initial_json)
}

#[tauri::command]
async fn cmd_send_gui_event(
    handler_id: i32,
    payload: String,
    state: tauri::State<'_, AppState>,
) -> Result<String, String> {
    let guard = state.guest.lock().unwrap();
    let handle = guard.as_ref().ok_or("No guest running")?;
    send_gui_event(handle, handler_id, payload)
}

#[tauri::command]
async fn cmd_stop_gui(state: tauri::State<'_, AppState>) -> Result<(), String> {
    if let Some(handle) = state.guest.lock().unwrap().take() {
        stop_gui(handle);
    }
    Ok(())
}

pub fn run() {
    tauri::Builder::default()
        .manage(AppState { guest: Mutex::new(None) })
        .invoke_handler(tauri::generate_handler![
            cmd_compile_run,
            cmd_run_gui,
            cmd_send_gui_event,
            cmd_stop_gui,
        ])
        .run(tauri::generate_context!())
        .expect("error running Vibe Studio");
}
```

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
vox = { path = "../../libs/vox/rust" }

[build-dependencies]
tauri-build = { version = "2" }
```

---

### S3: Svelte App Skeleton + Stores

**`studio/src/stores/ide.ts`**:

```typescript
import { writable } from 'svelte/store';

export interface FileEntry {
  name: string;
  content: string;
}

export interface IdeState {
  files: FileEntry[];
  activeIdx: number;
  code: string;
  output: string;
  isRunning: boolean;
  isGuiApp: boolean;
  guestRender: string;
  compileError: string;
}

const EXAMPLE_FILES: FileEntry[] = [
  {
    name: 'hello.vo',
    content: `package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, Vo!")\n}\n`,
  },
  {
    name: 'counter.vo',
    content: `package main\n\nimport "vogui"\n\ntype State struct { Count int }\n\nfunc view(state any) vogui.Node {\n    s := state.(*State)\n    return vogui.Column(\n        vogui.Text(fmt.Sprintf("Count: %d", s.Count)),\n        vogui.Button("+", vogui.On(inc)),\n    )\n}\n\nfunc inc(s *State) { s.Count++ }\n\nfunc main() {\n    vogui.Run(vogui.App{Init: func() any { return &State{} }, View: view})\n}\n`,
  },
];

export const ide = writable<IdeState>({
  files: EXAMPLE_FILES,
  activeIdx: 0,
  code: EXAMPLE_FILES[0].content,
  output: '',
  isRunning: false,
  isGuiApp: false,
  guestRender: '',
  compileError: '',
});
```

**`studio/src/App.svelte`**:

```svelte
<script lang="ts">
  import { onMount } from 'svelte';
  import Toolbar from './components/Toolbar.svelte';
  import FileTree from './components/FileTree.svelte';
  import Editor from './components/Editor.svelte';
  import OutputPanel from './components/OutputPanel.svelte';
  import PreviewPanel from './components/PreviewPanel.svelte';
  import { ide } from './stores/ide';
  import { initBridge } from './lib/bridge';
  import { actions } from './lib/actions';

  onMount(async () => {
    await initBridge();
  });

  $: isGuiApp = $ide.isGuiApp && $ide.guestRender !== '';
</script>

<div class="ide-root">
  <Toolbar />
  <div class="ide-main">
    <FileTree />
    <Editor value={$ide.code} on:change={(e) => actions.onEditorChange(e.detail)} />
    {#if isGuiApp}
      <PreviewPanel guestRender={$ide.guestRender} />
    {:else}
      <OutputPanel output={$ide.output} compileError={$ide.compileError} />
    {/if}
  </div>
</div>

<style>
  .ide-root { display: flex; flex-direction: column; height: 100vh; background: #1e1e2e; }
  .ide-main { display: flex; flex: 1; overflow: hidden; }
</style>
```

---

### S4: Bridge Abstraction

**`studio/src/lib/bridge.ts`**:

```typescript
export interface Bridge {
  compileRun(code: string): Promise<string>;
  runGui(code: string): Promise<string>;
  sendGuiEvent(handlerId: number, payload: string): Promise<string>;
  stopGui(): Promise<void>;
}

let _bridge: Bridge | null = null;

export function bridge(): Bridge {
  return _bridge!;
}

export async function initBridge(): Promise<void> {
  if ('__TAURI__' in window) {
    const { invoke } = await import('@tauri-apps/api/core');
    _bridge = {
      compileRun:   (code)               => invoke('cmd_compile_run', { code }),
      runGui:       (code)               => invoke('cmd_run_gui', { code }),
      sendGuiEvent: (handlerId, payload) => invoke('cmd_send_gui_event', { handlerId, payload }),
      stopGui:      ()                   => invoke('cmd_stop_gui'),
    };
  } else {
    const wasm = await import('../wasm/studio.js');
    await wasm.default();
    _bridge = {
      compileRun:   (code)               => Promise.resolve(wasm.compile_run(code)),
      runGui:       (code)               => Promise.resolve(wasm.run_gui(code)),
      sendGuiEvent: (handlerId, payload) => Promise.resolve(wasm.send_gui_event(handlerId, payload)),
      stopGui:      ()                   => { wasm.stop_gui(); return Promise.resolve(); },
    };
  }
}
```

**`studio/src/lib/actions.ts`**:

```typescript
import { ide } from '../stores/ide';
import { bridge } from './bridge';
import { get } from 'svelte/store';

function isGuiCode(code: string): boolean {
  return code.includes('"vogui"');
}

export const actions = {
  onEditorChange(code: string) {
    ide.update(s => ({ ...s, code }));
  },

  switchFile(idx: number) {
    ide.update(s => ({
      ...s,
      activeIdx: idx,
      code: s.files[idx].content,
    }));
  },

  async runCode() {
    const s = get(ide);
    ide.update(s => ({ ...s, isRunning: true, output: '', compileError: '', guestRender: '' }));

    try {
      if (isGuiCode(s.code)) {
        const json = await bridge().runGui(s.code);
        ide.update(s => ({ ...s, isRunning: true, isGuiApp: true, guestRender: json }));
      } else {
        const output = await bridge().compileRun(s.code);
        ide.update(s => ({ ...s, isRunning: false, isGuiApp: false, output }));
      }
    } catch (e: any) {
      ide.update(s => ({ ...s, isRunning: false, compileError: String(e) }));
    }
  },

  async stopCode() {
    await bridge().stopGui();
    ide.update(s => ({ ...s, isRunning: false, isGuiApp: false, guestRender: '' }));
  },
};
```

---

### S5: Editor.svelte (CodeMirror 6)

**`studio/src/components/Editor.svelte`**:

```svelte
<script lang="ts">
  import { onMount, onDestroy, createEventDispatcher } from 'svelte';
  import { EditorView, basicSetup } from 'codemirror';
  import { EditorState } from '@codemirror/state';
  import { go } from '@codemirror/lang-go';
  import { oneDark } from '@codemirror/theme-one-dark';

  export let value: string = '';

  const dispatch = createEventDispatcher<{ change: string }>();

  let container: HTMLDivElement;
  let view: EditorView;
  let suppressUpdate = false;

  onMount(() => {
    view = new EditorView({
      state: EditorState.create({
        doc: value,
        extensions: [
          basicSetup,
          go(),
          oneDark,
          EditorView.updateListener.of((update) => {
            if (update.docChanged && !suppressUpdate) {
              dispatch('change', update.state.doc.toString());
            }
          }),
        ],
      }),
      parent: container,
    });
  });

  $: if (view) {
    const current = view.state.doc.toString();
    if (current !== value) {
      suppressUpdate = true;
      view.dispatch({ changes: { from: 0, to: current.length, insert: value } });
      suppressUpdate = false;
    }
  }

  onDestroy(() => view?.destroy());
</script>

<div bind:this={container} class="editor-container"></div>

<style>
  .editor-container { flex: 1; overflow: hidden; display: flex; flex-direction: column; }
  .editor-container :global(.cm-editor) { height: 100%; }
  .editor-container :global(.cm-scroller) { overflow: auto; }
</style>
```

---

### S6: PreviewPanel.svelte

**`studio/src/components/PreviewPanel.svelte`**:

```svelte
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { renderNode } from '@vogui/renderer';
  import morphdom from 'morphdom';
  import { bridge } from '../lib/bridge';
  import { ide } from '../stores/ide';

  export let guestRender: string = '';

  let root: HTMLDivElement;
  let currentDom: Element | null = null;

  $: if (root && guestRender) {
    applyRender(guestRender);
  }

  function applyRender(json: string) {
    let parsed: any;
    try {
      parsed = JSON.parse(json);
    } catch {
      return;
    }
    const tree = parsed.tree ?? parsed;
    const newDom = renderNode(tree, {
      interactive: true,
      onEvent: async (handlerId: number, payload: string) => {
        try {
          const newJson = await bridge().sendGuiEvent(handlerId, payload);
          applyRender(newJson);
        } catch (e: any) {
          ide.update(s => ({ ...s, isGuiApp: false, guestRender: '', output: String(e) }));
        }
      },
    });
    if (!newDom) return;
    if (currentDom) {
      morphdom(currentDom, newDom);
    } else {
      root.appendChild(newDom);
      currentDom = newDom as Element;
    }
  }

  onDestroy(() => {
    root?.replaceChildren();
    currentDom = null;
  });
</script>

<div bind:this={root} class="preview-panel"></div>

<style>
  .preview-panel {
    flex: 0 0 400px;
    overflow: auto;
    background: #181825;
    border-left: 1px solid #313244;
  }
</style>
```

---

### S7: Remaining Svelte Components

**`studio/src/components/Toolbar.svelte`**:

```svelte
<script lang="ts">
  import { ide } from '../stores/ide';
  import { actions } from '../lib/actions';

  $: activeFile = $ide.files[$ide.activeIdx]?.name ?? '';
  $: isRunning = $ide.isRunning;
</script>

<div class="toolbar">
  <button class="btn-run" on:click={actions.runCode} disabled={isRunning}>
    {isRunning ? '⏳ Running…' : '▶ Run'}
  </button>
  <button class="btn-stop" on:click={actions.stopCode} disabled={!isRunning}>
    ■ Stop
  </button>
  <span class="filename">{activeFile}</span>
  <span class="spacer"></span>
  <span class="title">Vibe Studio</span>
</div>

<style>
  .toolbar { display: flex; align-items: center; gap: 8px; padding: 8px 12px;
             background: #181825; border-bottom: 1px solid #313244; flex-shrink: 0; }
  .btn-run  { background: #22c55e; color: #fff; border: none; border-radius: 4px;
              padding: 4px 12px; cursor: pointer; font-weight: 600; }
  .btn-stop { background: #ef4444; color: #fff; border: none; border-radius: 4px;
              padding: 4px 12px; cursor: pointer; font-weight: 600; }
  .filename { color: #888; font-size: 13px; }
  .spacer   { flex: 1; }
  .title    { color: #cdd6f4; font-weight: 700; font-size: 14px; }
</style>
```

**`studio/src/components/FileTree.svelte`**:

```svelte
<script lang="ts">
  import { ide } from '../stores/ide';
  import { actions } from '../lib/actions';
</script>

<div class="filetree">
  <div class="label">Files</div>
  {#each $ide.files as file, i}
    <button
      class="file-entry"
      class:active={i === $ide.activeIdx}
      on:click={() => actions.switchFile(i)}
    >
      {file.name}
    </button>
  {/each}
</div>

<style>
  .filetree  { width: 200px; background: #181825; border-right: 1px solid #313244;
               padding: 8px; display: flex; flex-direction: column; gap: 2px; flex-shrink: 0; }
  .label     { color: #888; font-size: 11px; font-weight: 700; padding: 4px 6px;
               text-transform: uppercase; letter-spacing: 0.05em; }
  .file-entry { background: none; border: none; color: #cdd6f4; text-align: left;
                padding: 6px 8px; border-radius: 4px; cursor: pointer; font-size: 13px; width: 100%; }
  .file-entry:hover { background: #24273a; }
  .file-entry.active { background: #313244; color: #cba6f7; }
</style>
```

**`studio/src/components/OutputPanel.svelte`**:

```svelte
<script lang="ts">
  export let output: string = '';
  export let compileError: string = '';
</script>

<div class="output-panel">
  <div class="label">Output</div>
  {#if compileError}
    <pre class="error">{compileError}</pre>
  {:else}
    <pre class="output">{output || '(no output)'}</pre>
  {/if}
</div>

<style>
  .output-panel { flex: 0 0 400px; background: #181825; border-left: 1px solid #313244;
                  display: flex; flex-direction: column; overflow: hidden; }
  .label   { color: #888; font-size: 11px; font-weight: 700; padding: 8px 12px;
             border-bottom: 1px solid #313244; text-transform: uppercase; }
  .output, .error { flex: 1; padding: 12px; margin: 0; overflow: auto; font-size: 13px;
                    font-family: 'JetBrains Mono', monospace; white-space: pre-wrap; }
  .output { color: #cdd6f4; }
  .error  { color: #f38ba8; }
</style>
```

---

### S8: WASM Mode

**`studio/wasm/src/lib.rs`**:

```rust
use wasm_bindgen::prelude::*;
use vox::gui::{GuestHandle, compile_run, run_gui, send_gui_event, stop_gui};
use std::cell::RefCell;

thread_local! {
    static GUEST: RefCell<Option<GuestHandle>> = RefCell::new(None);
}

#[wasm_bindgen]
pub fn compile_run(code: &str) -> Result<String, JsValue> {
    vox::gui::compile_run(code).map_err(|e| JsValue::from_str(&e))
}

#[wasm_bindgen]
pub fn run_gui(code: &str) -> Result<String, JsValue> {
    let (initial_json, handle) = vox::gui::run_gui(code).map_err(|e| JsValue::from_str(&e))?;
    GUEST.with(|g| *g.borrow_mut() = Some(handle));
    Ok(initial_json)
}

#[wasm_bindgen]
pub fn send_gui_event(handler_id: i32, payload: &str) -> Result<String, JsValue> {
    GUEST.with(|g| {
        let borrow = g.borrow();
        let handle = borrow.as_ref().ok_or_else(|| JsValue::from_str("No guest running"))?;
        vox::gui::send_gui_event(handle, handler_id, payload.to_string())
            .map_err(|e| JsValue::from_str(&e))
    })
}

#[wasm_bindgen]
pub fn stop_gui() {
    GUEST.with(|g| {
        if let Some(handle) = g.borrow_mut().take() {
            vox::gui::stop_gui(handle);
        }
    });
}
```

> **Note**: In WASM, `run_gui` cannot spawn OS threads. The guest VM runs synchronously on the WASM thread — `run_until_blocked` returns when the fiber suspends on its event channel. This works because WASM vogui apps block on an `mpsc::channel::recv()` which in a single-threaded WASM context means "I've emitted my render, return to caller." The guest loop for WASM is: run → blocked → return render JSON → JS calls send_gui_event → guest resumes → blocked → return render JSON.

---

## 4. Directory Layout (Final)

```
studio/
├── src/                          # Svelte frontend
│   ├── App.svelte                # Root layout + bridge init
│   ├── components/
│   │   ├── Toolbar.svelte        # Run/Stop, file name, status
│   │   ├── FileTree.svelte       # File list
│   │   ├── Editor.svelte         # CodeMirror 6 native component
│   │   ├── OutputPanel.svelte    # Console output + compile errors
│   │   └── PreviewPanel.svelte   # Guest vogui app via renderNode
│   ├── stores/
│   │   └── ide.ts                # IdeState writable store + example files
│   ├── lib/
│   │   ├── bridge.ts             # Tauri | WASM abstraction
│   │   └── actions.ts            # runCode, stopCode, switchFile, onEditorChange
│   └── main.ts                   # Svelte mount
├── src-tauri/                    # Tauri Rust backend
│   ├── Cargo.toml
│   ├── tauri.conf.json
│   ├── build.rs
│   └── src/
│       ├── main.rs               # fn main() → lib::run()
│       └── lib.rs                # AppState + Tauri commands
├── wasm/                         # WASM entry point (for web mode)
│   ├── Cargo.toml
│   └── src/lib.rs                # compile_run, run_gui, send_gui_event, stop_gui exports
├── package.json
├── vite.config.ts
└── svelte.config.ts
```

Changes in existing code:

```
libs/vox/
├── vox.vo                        # MODIFIED: add RunGui, SendGuiEvent, StopGui, CompileRun
└── rust/src/
    ├── gui.rs                    # NEW: GuestHandle, run_gui, send_gui_event, stop_gui, compile_run
    └── ffi.rs                    # MODIFIED: register new vox externs
```

No changes to `libs/vogui/` at all. The vogui/js renderer is imported as a package by `PreviewPanel.svelte` — no new features needed.

---

## 5. Build & Run

### Native (Tauri)

```bash
cd studio
npm install                        # svelte, codemirror, morphdom, @tauri-apps/api
cargo tauri dev                    # Builds Rust + starts Vite dev server + opens window
```

### Web (WASM)

```bash
cd studio/wasm
wasm-pack build --target web --out-dir ../src/wasm
cd ..
npm run dev                        # Vite serves src/ + WASM binary
```

---

## 6. Milestone Criteria (Phase 1 Done)

- [ ] Tauri app launches, shows Svelte IDE (dark theme, three-panel layout)
- [ ] CodeMirror editor loads with syntax highlighting, edits update store
- [ ] File tree shows example files, clicking switches editor content
- [ ] Click "Run" on console app → `compile_run` → output in OutputPanel
- [ ] Click "Run" on vogui app → `run_gui` → guest renders in PreviewPanel
- [ ] Guest app events work (click button → `send_gui_event` → guest re-renders)
- [ ] Click "Stop" → `stop_gui` → guest thread exits, panel cleared
- [ ] Web WASM mode: same UI in browser, no Tauri

---

## 7. What's NOT in Phase 1

- Real filesystem (open folder, save to disk) — Phase 1 uses embedded example files
- LSP / inline diagnostics in editor
- Terminal emulator (xterm)
- Split pane resizing (fixed widths in Phase 1)
- Hot reload
- Debug mode / breakpoints
- Settings / preferences
- Project management (open folder, vo.mod, multi-file compilation)
- Mobile responsive layout
