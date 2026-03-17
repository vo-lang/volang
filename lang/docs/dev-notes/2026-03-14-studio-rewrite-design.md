# Studio Rewrite — Design Document

> Status: Draft  
> Date: 2026-03-14  
> Supersedes: `2026-03-14-studio-first-principles-refactor-plan.md` (partial), `2026-03-14-vox-stdlib-studio-voplay-refactor-plan.md` (Studio section)

---

## 1. Positioning

Studio is **two products in one binary/deployment**:

| Mode | Purpose | UI Surface |
|------|---------|------------|
| **Dev Mode** | Full IDE: editor, file explorer, terminal, preview, console | All panels visible |
| **Runner Mode** | Execute a Vo project with minimal chrome | Preview + console only |

Mode is determined by URL parameter or CLI argument at launch.  
Both modes share the same build artifact — no separate "runner" binary.

### 1.1 Project Sources

Studio accepts projects from three origins:

| Origin | Dev Mode | Runner Mode | Resolution |
|--------|----------|-------------|------------|
| **Studio-local** | Default workspace (`~/.vibe-studio/workspace`) | N/A | Direct filesystem (native) or OPFS/VFS (web) |
| **External path** | `--path /abs/path` or UI "Open Folder" | `--path /abs/path` | Native: real FS. Web: not supported (no local FS access) |
| **URL** | `--url https://...` or UI "Open URL" | `?url=https://...` | Fetch project archive → extract to session workspace |

External-path projects are **not** copied into Studio's workspace. The session root is set
to the external path directly. This avoids the current `materialize_local_target` copy dance.

---

## 2. Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    Studio App                            │
│                                                          │
│  ┌────────────────────────────────────────────────────┐  │
│  │              Frontend (Svelte)                      │  │
│  │  ┌──────────┐ ┌──────────┐ ┌───────────────────┐  │  │
│  │  │  Editor   │ │  File    │ │  Terminal          │  │  │
│  │  │ (Monaco)  │ │  Explorer│ │  (xterm.js)        │  │  │
│  │  └──────────┘ └──────────┘ └───────────────────┘  │  │
│  │  ┌──────────────────────┐  ┌───────────────────┐  │  │
│  │  │  Preview Panel       │  │  Console Output   │  │  │
│  │  │  (GUI Host Surface)  │  │                   │  │  │
│  │  └──────────────────────┘  └───────────────────┘  │  │
│  └────────────────────────────────────────────────────┘  │
│                          │                                │
│                    Service Layer                          │
│  ┌────────────────────────────────────────────────────┐  │
│  │  WorkspaceService  │  CompilerService              │  │
│  │  TerminalService   │  RuntimeService               │  │
│  │  ExtensionService  │  ProjectService               │  │
│  └────────────────────────────────────────────────────┘  │
│                          │                                │
│                    Backend Interface                      │
│  ┌──────────────────┐  ┌──────────────────────────────┐  │
│  │  NativeBackend    │  │  WasmBackend                 │  │
│  │  (Tauri/Rust)     │  │  (vo-web + VFS)              │  │
│  └──────────────────┘  └──────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### 2.1 Design Principles

1. **Service-oriented frontend** — Each concern is a service with a typed interface. No god objects.
2. **Backend interface, not backend abstraction** — Services call a `Backend` interface. Two implementations: `NativeBackend` (Tauri IPC) and `WasmBackend` (in-browser). No runtime detection sprinkled everywhere.
3. **Terminal is a subsystem, not the base abstraction** — Terminal is one panel powered by services, not the routing layer for everything.
4. **GUI hosting is generic** — Studio provides surface + event dispatch + capability injection. Framework-specific knowledge stays in the framework's own code.
5. **State follows product domains** — Not one flat store. Each service owns its domain state.

---

## 3. Service Layer

### 3.1 WorkspaceService

Owns the filesystem abstraction for the current session.

```typescript
interface WorkspaceService {
  // Session
  readonly root: string;
  readonly projectMode: 'single-file' | 'module';

  // Filesystem
  list(path: string): Promise<FsEntry[]>;
  stat(path: string): Promise<FsStat>;
  read(path: string): Promise<string>;
  readMany(paths: string[]): Promise<ReadManyResult[]>;
  write(path: string, content: string): Promise<void>;
  mkdir(path: string): Promise<void>;
  remove(path: string, recursive: boolean): Promise<void>;
  rename(oldPath: string, newPath: string): Promise<void>;
  copy(src: string, dst: string): Promise<void>;
  grep(path: string, pattern: string, opts?: GrepOpts): Promise<GrepMatch[]>;

  // Watchers (native only — web polls or uses OPFS observers)
  watch(path: string, cb: (events: FsEvent[]) => void): Disposable;
}
```

**Native**: delegates to real filesystem via Tauri IPC.  
**WASM**: delegates to OPFS-backed VFS (`vo_web_runtime_wasm::vfs`).

### 3.2 ProjectService

Manages project resolution, module discovery, and session lifecycle.

```typescript
interface ProjectService {
  // Session lifecycle
  openLocal(path: string): Promise<SessionInfo>;
  openUrl(url: string): Promise<SessionInfo>;
  openWorkspace(): Promise<SessionInfo>;
  closeSession(): Promise<void>;

  // Project introspection
  readonly sessionInfo: SessionInfo;
  findProjectRoot(entryPath: string): Promise<string | null>;
  readModFile(projectRoot: string): Promise<ModFile>;
  listDependencies(projectRoot: string): Promise<Dependency[]>;
}

interface SessionInfo {
  root: string;
  origin: 'workspace' | 'local-path' | 'url';
  projectMode: 'single-file' | 'module';
  entryPath: string | null;
}
```

### 3.3 CompilerService

Compile, check, format, build — thin typed wrapper over backend.

```typescript
interface CompilerService {
  check(path: string): Promise<CheckResult>;
  compile(path: string): Promise<CompileResult>;
  format(path: string): Promise<string>;
  build(path: string, output?: string): Promise<BuildResult>;
  dump(path: string): Promise<string>;
}
```

**Native**: `vo-engine` compile/check/format/build via Tauri commands.  
**WASM**: `vo-web` compile via `wasm/src/lib.rs` exports.

### 3.4 RuntimeService

Runs Vo programs — both console and GUI apps.

```typescript
interface RuntimeService {
  // Console execution
  run(path: string, opts?: RunOpts): AsyncGenerator<RunEvent>;
  stop(): void;

  // GUI execution
  runGui(path: string): Promise<GuiSession>;
}

interface GuiSession {
  readonly initialRender: Uint8Array;
  readonly moduleBytes: Uint8Array;      // for render-island if needed
  readonly entryPath: string;
  readonly capabilities: FrameworkCapabilities;

  sendEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  sendEventAsync(handlerId: number, payload: string): Promise<void>;
  pushIslandData(data: Uint8Array): Promise<void>;
  onPushRender(cb: (bytes: Uint8Array) => void): Disposable;
  onIslandData(cb: (data: Uint8Array) => void): Disposable;
  stop(): void;
}

interface RunOpts {
  mode?: 'vm' | 'jit';
  args?: string[];
  stdin?: string;
}

type RunEvent =
  | { kind: 'stdout'; text: string }
  | { kind: 'stderr'; text: string }
  | { kind: 'done';   exitCode: number; durationMs: number }
  | { kind: 'error';  message: string };
```

**Native**: console runs via `vo-engine::run_with_output` in a background thread, GUI runs via `gui_runtime::run_gui`.  
**WASM**: console runs via `compile_run_entry`, GUI runs via `run_gui_entry` + `send_gui_event`.

### 3.5 ExtensionService

Manages third-party module installation, native extension compilation, and WASM extension loading.

```typescript
interface ExtensionService {
  // Module installation
  install(spec: string): AsyncGenerator<InstallEvent>;  // "module@version"
  listInstalled(): Promise<InstalledModule[]>;
  remove(module: string): Promise<void>;

  // Extension status
  isBuilt(module: string): Promise<boolean>;
  buildStatus(module: string): Promise<BuildStatus>;

  // WASM extension loading (web only)
  loadWasmExtension(module: string, wasmBytes: Uint8Array, jsGlueUrl?: string): Promise<void>;
}

type InstallEvent =
  | { kind: 'fetch';   message: string }
  | { kind: 'build';   line: string }           // cargo build output
  | { kind: 'done';    module: string }
  | { kind: 'error';   message: string };
```

See **Section 7** for the full extension build pipeline.

### 3.6 TerminalService

Provides a Vo command runner with IDE-like UX.

```typescript
interface TerminalService {
  // Command execution
  execute(input: string): AsyncGenerator<TerminalEvent>;
  interrupt(): void;

  // History & completion
  history(): string[];
  complete(partial: string): Promise<Completion[]>;

  // Session
  readonly cwd: string;
  cd(path: string): void;
  env(): Record<string, string>;
  setEnv(key: string, value: string): void;
}

type TerminalEvent =
  | { kind: 'stdout'; text: string }
  | { kind: 'stderr'; text: string }
  | { kind: 'done';   exitCode: number }
  | { kind: 'prompt'; cwd: string };
```

See **Section 6** for terminal architecture details.

---

## 4. Backend Interface

The service layer never touches Tauri or WASM APIs directly. All platform calls go through
a single `Backend` interface:

```typescript
interface Backend {
  readonly platform: 'native' | 'wasm';

  // Filesystem
  fsList(path: string): Promise<FsEntry[]>;
  fsStat(path: string): Promise<FsStat>;
  fsRead(path: string): Promise<string>;
  fsReadMany(paths: string[]): Promise<ReadManyResult[]>;
  fsWrite(path: string, content: string): Promise<void>;
  fsMkdir(path: string): Promise<void>;
  fsRemove(path: string, recursive: boolean): Promise<void>;
  fsRename(oldPath: string, newPath: string): Promise<void>;
  fsCopy(src: string, dst: string): Promise<void>;
  fsGrep(path: string, pattern: string, opts?: GrepOpts): Promise<GrepMatch[]>;

  // Compiler
  check(target: string): Promise<CheckResult>;
  compile(target: string): Promise<CompileResult>;
  format(path: string): Promise<string>;
  build(target: string, output?: string): Promise<BuildResult>;
  dump(path: string): Promise<string>;

  // Runtime
  run(target: string, opts: RunOpts): StreamHandle<RunEvent>;
  runGui(target: string): Promise<GuiSessionHandle>;

  // Toolchain
  voGet(spec: string): StreamHandle<InstallEvent>;
  voInit(name?: string): Promise<string>;
  voVersion(): Promise<string>;

  // Process (native only)
  spawn(program: string, args: string[], env?: Record<string, string>): StreamHandle<ProcEvent>;

  // HTTP
  httpRequest(method: string, url: string, opts?: HttpOpts): Promise<HttpResult>;

  // Git
  gitExec(op: GitOp): Promise<GitResult> | StreamHandle<GitEvent>;

  // Extension management
  loadWasmExt(key: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void>;
  callExt(name: string, input: Uint8Array): Uint8Array;
}
```

### 4.1 NativeBackend

Implementation: Tauri IPC commands.

Each method maps to a `#[tauri::command]` in `src-tauri/src/`. Unlike the current design where
`lib.rs` is a 960-line monolith, commands are organized into modules:

```
src-tauri/src/
  lib.rs              — Tauri setup, AppState, plugin registration
  commands/
    workspace.rs      — fs.* commands
    compiler.rs       — compile, check, format, build, dump
    runtime.rs        — run, run_gui, gui events, stop
    extension.rs      — vo get, extension build, load
    terminal.rs       — process spawn, env management
    git.rs            — git operations
    http.rs           — HTTP proxy
  gui_runtime.rs      — GUI VM thread (cleaned, no voplay-specific code)
```

### 4.2 WasmBackend

Implementation: direct calls to `wasm/pkg/` exports + VFS + in-browser Vo VM.

The current `bridge.ts` responsibilities are dissolved:
- Runtime detection → gone (Backend is injected at app init)
- Shell handler bytecode caching → moves to `WasmBackend` internal
- FS convenience → `WorkspaceService`
- GUI render callback → `RuntimeService`
- WASM host bridge → `ExtensionService`
- Dev-mode module loading → `ProjectService` + `ExtensionService`

---

## 5. State Architecture

Current problem: one flat `IdeState` store mixes workspace, editor, console, execution, and
GUI state. New design: **each service owns its domain state** as a Svelte store.

```typescript
// Workspace state — owned by WorkspaceService
interface WorkspaceState {
  root: string;
  dirCache: Map<string, FsEntry[]>;
  expandedDirs: Set<string>;
}

// Editor state — owned by EditorController (UI-level, not a service)
interface EditorState {
  activeFilePath: string;
  openFiles: OpenFile[];
  dirty: Set<string>;
}

// Session state — owned by ProjectService
interface SessionState {
  info: SessionInfo;
  mode: 'dev' | 'runner';
}

// Runtime state — owned by RuntimeService
interface RuntimeState {
  status: 'idle' | 'running' | 'done' | 'error';
  kind: 'console' | 'gui' | null;
  durationMs: number | null;
  guiSession: GuiSession | null;
}

// Console state — owned by ConsoleController (UI-level)
interface ConsoleState {
  lines: ConsoleLine[];
  showTimestamps: boolean;
  wordWrap: boolean;
}

// Terminal state — owned by TerminalService
interface TerminalState {
  history: string[];
  cwd: string;
  env: Record<string, string>;
}
```

---

## 6. Terminal Architecture

The terminal is a **Vo command runner**, not a general-purpose shell.

### 6.1 Command Grammar

```
<command> ::= <program> <args>*
<program> ::= "vo" | "git" | "cat" | "ls" | "cd" | "pwd" | "mkdir" | "rm" | "cp" | "mv"
            | "grep" | "echo" | "clear" | "help" | "exit"
<args>    ::= <flag> | <path> | <string>
```

No pipes, no redirects, no variables, no scripting. This is intentional — Studio is not a
general-purpose shell. It's a Vo development tool.

### 6.2 Command Mapping

| Terminal Command | Service Call |
|-----------------|-------------|
| `vo run main.vo` | `RuntimeService.run("main.vo", { mode: "vm" })` |
| `vo run --mode=jit main.vo` | `RuntimeService.run("main.vo", { mode: "jit" })` |
| `vo check .` | `CompilerService.check(".")` |
| `vo build .` | `CompilerService.build(".")` |
| `vo format main.vo` | `CompilerService.format("main.vo")` |
| `vo get mod@v1.0` | `ExtensionService.install("mod@v1.0")` |
| `vo init` | Backend.voInit() |
| `vo test` | Backend.run (streaming) |
| `git status` | Backend.gitExec({ kind: 'git.status' }) |
| `git add .` | Backend.gitExec({ kind: 'git.add', paths: ['.'] }) |
| `ls [path]` | `WorkspaceService.list(path)` |
| `cat <file>` | `WorkspaceService.read(file)` |
| `cd <path>` | `TerminalService.cd(path)` |
| `mkdir <path>` | `WorkspaceService.mkdir(path)` |
| `rm [-r] <path>` | `WorkspaceService.remove(path, recursive)` |
| `cp <src> <dst>` | `WorkspaceService.copy(src, dst)` |
| `mv <old> <new>` | `WorkspaceService.rename(old, new)` |
| `grep <pattern> <path>` | `WorkspaceService.grep(path, pattern)` |

### 6.3 UI Component

- **xterm.js** for terminal rendering (proper terminal emulator in browser)
- Line editing, cursor movement, ANSI color support
- Tab completion via `TerminalService.complete()`
- Command history via up/down arrows
- `Ctrl+C` sends `TerminalService.interrupt()`

### 6.4 Command Parser

A simple hand-written parser. Not a shell language — just command + args with basic quoting:

```typescript
interface ParsedCommand {
  program: string;
  args: string[];
}

function parseCommand(input: string): ParsedCommand { ... }
```

Supports:
- Bare words: `vo run main.vo`
- Single quotes: `echo 'hello world'`
- Double quotes: `echo "hello world"`
- Backslash escapes: `echo hello\ world`
- `--flag=value` and `--flag value` styles

### 6.5 Native-Specific: Process Spawn

On native backend, the terminal also supports `proc.spawn` for arbitrary commands.
This enables `cargo build`, `rustup`, etc. — needed for extension development workflows.

```
> cargo build --release --manifest-path ./rust/Cargo.toml
   Compiling my-ext v0.1.0
    Finished release [optimized] target(s) in 12.34s
```

On WASM backend, `proc.spawn` is not available. Attempting it shows a clear error:
`error: process execution is not available in web mode`.

---

## 7. Extension Build Pipeline

### 7.1 Module Structure Convention

A Vo module with native extensions has this structure:

```
github.com/vo-lang/zip/
  vo.mod            — module declaration
  vo.ext.toml       — extension manifest (declares Rust crate)
  zip.vo            — Vo source
  rust/
    Cargo.toml      — Rust crate
    src/
      lib.rs        — native extension code
  zip.wasm          — pre-built WASM (checked into repo or GitHub release)
```

### 7.2 `vo get` Flow

```
vo get github.com/vo-lang/zip@v0.1.0
```

#### Phase 1: Fetch Source

1. Resolve module spec → GitHub archive URL
2. Download + extract to `~/.vo/mod/github.com/vo-lang/zip@v0.1.0/`
3. Parse `vo.mod` and recursively install dependencies

#### Phase 2: Build Native Extension (native backend only)

1. Check `vo.ext.toml` — if absent, done (pure Vo module)
2. Check build cache: `~/.vo/cache/ext/github.com/vo-lang/zip@v0.1.0/<target_triple>/<rustc_version>/`
   - If cache hit (`.dylib`/`.so`/`.dll` exists and is valid), done
3. Run `cargo build --release --manifest-path <module>/rust/Cargo.toml --target-dir <cache_dir>`
4. Stream `cargo` output → terminal (via `InstallEvent { kind: 'build', line }`)
5. On success, copy artifact to cache
6. On failure, report error with full cargo output

#### Phase 3: Load WASM Extension (web backend only)

1. Check `vo.ext.toml` — if absent, done
2. Look for `<module>.wasm` in module directory
3. If present, load via `ExtensionService.loadWasmExtension()`
4. If absent, check GitHub release for pre-built WASM
5. If no WASM available, report: `warning: native extension '...' has no WASM build; extern calls will fail in web mode`

### 7.3 Build Cache Structure

```
~/.vo/
  mod/                                    — module source cache
    github.com/vo-lang/zip@v0.1.0/
      vo.mod
      zip.vo
      rust/
        Cargo.toml
        src/lib.rs
      zip.wasm
  cache/
    ext/                                  — compiled extension cache
      github.com/vo-lang/zip@v0.1.0/
        aarch64-apple-darwin/
          1.85.0/                         — rustc version
            libzip.dylib
            build.log
        x86_64-unknown-linux-gnu/
          1.85.0/
            libzip.so
```

Cache key: `(module@version, target_triple, rustc_version)`.  
`vo clean` clears the cache. `vo get --rebuild` forces recompilation.

### 7.4 Extension Loading at Runtime

When `CompilerService.compile()` or `RuntimeService.run()` is called:

1. Compiler resolves all dependencies from `vo.mod`
2. For each dependency with `vo.ext.toml`:
   - **Native**: locate compiled `.dylib`/`.so` from cache → `ExtensionLoader::load()`
   - **WASM**: locate loaded WASM module → `ext_bridge` lookup
3. Extensions are passed to the VM at load time

---

## 8. GUI Framework Contract

### 8.1 Purpose

Studio provides a generic GUI hosting environment. Any Vo UI framework that conforms to the
contract can be hosted in Studio's preview panel.

Frameworks that don't want Studio hosting (e.g., they create their own native window) are
free to ignore this entirely — they just run as console apps from Studio's perspective.

### 8.2 Contract Declaration

In `vo.ext.toml`:

```toml
[studio]
entry = "Run"                      # function Studio calls to start the app
capabilities = [
  "timer",                         # setTimeout, setInterval, requestAnimationFrame
  "render_byte_stream",            # host_output-based rendering (vogui style)
  "render_surface",                # canvas/WebGPU surface (voplay style)
  "input_events",                  # keyboard, mouse, touch
  "island_transport",              # multi-VM island communication
  "game_loop",                     # fixed-timestep game loop
]
```

If no `[studio]` section exists, Studio treats the app as a console app.

### 8.3 Capability Definitions

#### `timer`

Host provides timer scheduling to the guest VM:

| Operation | Mechanism |
|-----------|-----------|
| `setTimeout(ms)` | Host event with `delay_ms`, handler_id = -1 |
| `setInterval(ms)` | Repeating host event |
| `clearTimeout(id)` | Cancel pending timer |
| `requestAnimationFrame` | Host event with ~16ms delay, handler_id = -4 |

Implementation:
- **Native**: `NativeGuiPlatform` spawns threads with `recv_timeout` (current approach, cleaned)
- **WASM**: JS `setTimeout`/`setInterval`/`requestAnimationFrame` → `wakeHostEvent`

#### `render_byte_stream`

Framework renders by writing bytes to `host_output`. Studio reads the bytes after each
event dispatch and forwards them to the preview panel.

- Preview panel interprets bytes according to the framework's protocol
- For **vogui**: bytes are serialized widget tree → JS renderer decodes and renders to DOM
- For other frameworks: Studio provides raw byte passthrough, framework provides its own
  JS renderer module

The JS renderer module is declared in `vo.ext.toml`:

```toml
[studio]
renderer = "js/dist/renderer.js"   # JS module that receives render bytes
```

Studio loads this module and calls `render(container: HTMLElement, bytes: Uint8Array)`.

If no `renderer` is specified, Studio shows raw bytes in a hex viewer (development/debug mode).

#### `render_surface`

Framework needs a GPU/canvas surface. Studio provides:

- An `<canvas>` element in the preview panel
- Canvas ID communicated via initial event payload
- Resize events when the panel resizes

The framework's native extension (WASM or native) directly draws to the canvas/surface.

For **voplay**: the render-island architecture stays — main VM on native backend,
render island WASM VM in browser, communicating via island transport. Studio just provides
the canvas and transport relay without knowing it's voplay.

#### `input_events`

Host captures input on the preview panel and delivers to the guest VM:

- Keyboard: keydown, keyup, keypress
- Mouse: move, click, scroll, drag
- Touch: start, move, end
- Gamepad: button, axis (if `game_loop` is also declared)

Events are encoded as JSON and delivered via the standard event dispatch path
(`handler_id` + `payload`).

#### `island_transport`

Host provides island-to-island communication relay:

- Guest VM emits outbound island frames via `take_outbound_commands()`
- Host relays frames:
  - **Native**: Tauri event `island_data` → frontend → render island WASM VM
  - **WASM**: direct queue between main WASM VM and render island WASM VM
- Inbound frames are pushed to guest VM via `push_island_command()`

Studio does not interpret island frame contents. It's an opaque byte relay.

#### `game_loop`

Host provides a fixed-timestep game loop:

- Spawns a loop thread/timer at ~60fps
- Delivers `{ Dt: <ms> }` events with handler_id = -5
- Guest processes physics/logic per frame

### 8.4 Widget Registration

For frameworks using `render_byte_stream` with a custom renderer, the renderer JS module
can register **widgets** — interactive DOM elements that the framework's render output
can reference.

```typescript
// In the framework's renderer.js
export function init(host: StudioGuiHost): void {
  host.registerWidget('my-framework-canvas', {
    create(container, props, onEvent) { ... },
  });
}

export function render(container: HTMLElement, bytes: Uint8Array): void {
  // Decode bytes and update DOM
}
```

Studio's `StudioGuiHost` interface:

```typescript
interface StudioGuiHost {
  registerWidget(name: string, factory: WidgetFactory): void;
  getCanvas(): HTMLCanvasElement | null;  // for render_surface capability
  sendEvent(handlerId: number, payload: string): Promise<Uint8Array>;
}
```

This replaces the current hardcoded `registerWidget('voplay-render-island', ...)` in
`render_island.ts`. Studio no longer has any voplay-specific code.

### 8.5 Discovery Flow

When `RuntimeService.runGui(path)` is called:

1. Compile the project
2. Inspect `vo.ext.toml` of the main module (and its dependencies)
3. Collect `[studio]` declarations → determine capabilities needed
4. Configure the preview panel:
   - If `render_byte_stream` + `renderer`: load the renderer JS module
   - If `render_surface`: create canvas element
   - If `island_transport`: set up transport relay
5. Start the VM with appropriate configuration
6. Call the `entry` function (default: `Run`)
7. Begin event dispatch loop

---

## 9. Frontend Component Architecture

### 9.1 App Shell

```
<App>
  ├── <SessionProvider>           — provides SessionState context
  │   ├── <DevModeLayout>         — shown when mode='dev'
  │   │   ├── <Sidebar>
  │   │   │   ├── <FileExplorer>  — uses WorkspaceService
  │   │   │   └── <ExtensionPanel> — uses ExtensionService
  │   │   ├── <MainArea>
  │   │   │   ├── <EditorTabs>    — uses EditorController
  │   │   │   └── <Editor>        — Monaco, uses WorkspaceService for save/load
  │   │   ├── <BottomPanel>
  │   │   │   ├── <TerminalPanel> — xterm.js, uses TerminalService
  │   │   │   └── <ConsolePanel>  — uses ConsoleController
  │   │   └── <PreviewPanel>      — uses RuntimeService
  │   │
  │   └── <RunnerModeLayout>      — shown when mode='runner'
  │       ├── <PreviewPanel>
  │       └── <ConsolePanel>
  │
  └── <ServiceRegistry>           — DI container for all services
```

### 9.2 Service Initialization

At app startup:

```typescript
// Determine backend
const backend = isTauri()
  ? new NativeBackend()
  : new WasmBackend();

// Create services
const workspace  = new WorkspaceService(backend);
const project    = new ProjectService(backend, workspace);
const compiler   = new CompilerService(backend);
const runtime    = new RuntimeService(backend, compiler);
const extension  = new ExtensionService(backend);
const terminal   = new TerminalService(backend, workspace, compiler, runtime, extension);

// Initialize session
const mode = detectMode();  // URL params / CLI args
if (mode.url) {
  await project.openUrl(mode.url);
} else if (mode.path) {
  await project.openLocal(mode.path);
} else {
  await project.openWorkspace();
}
```

No runtime detection scattered through the codebase. The `Backend` is chosen once, injected
everywhere.

---

## 10. Native Backend (Tauri) Structure

### 10.1 Rust Module Layout

```
studio/src-tauri/src/
  lib.rs                  — Tauri plugin setup, AppState
  state.rs                — AppState definition, session management
  commands/
    mod.rs                — re-exports all command modules
    workspace.rs          — fs operations (list, stat, read, write, mkdir, etc.)
    compiler.rs           — compile, check, format, build, dump
    runtime.rs            — run console app, run GUI app, send events, stop
    extension.rs          — vo get, extension build, native ext loading
    process.rs            — spawn subprocess (for terminal proc.spawn)
    git.rs                — git operations
    http.rs               — HTTP proxy
  gui_runtime.rs          — generic GUI VM host (no framework-specific code)
  ext_builder.rs          — cargo build orchestration for native extensions
```

### 10.2 AppState

```rust
pub struct AppState {
    workspace_root: PathBuf,
    session: Mutex<Session>,
    gui_host: Mutex<Option<GuiHost>>,
}

struct Session {
    root: PathBuf,
    origin: SessionOrigin,
}

enum SessionOrigin {
    Workspace,
    LocalPath(PathBuf),
    Url(String),
}
```

No more `shell_runner: shell::VoRunner` in AppState. The Vo shell handler program is removed
from the native backend — it was only needed because the old design routed everything through
a single shell handler Vo program. Now each command module handles its own operations directly.

### 10.3 gui_runtime.rs Cleanup

The current `gui_runtime.rs` has voplay-specific code:

```rust
// REMOVE: voplay-specific studio mode configuration
fn configure_loaded_extensions(loader: &ExtensionLoader) -> Result<bool, String> {
    unsafe {
        if let Some(set_mode) = loader
            .symbol::<unsafe extern "C" fn(bool)>("voplay", b"vo_voplay_set_studio_mode\0")
            ...
    }
}
```

New design: `gui_runtime.rs` is framework-agnostic. Framework-specific initialization is
the framework's own responsibility, triggered through its extern functions during VM startup.

The `NativeGuiPlatform` trait implementation stays (it's already generic — provides timer
scheduling). The `VoguiPlatform` trait name should be renamed to a generic `GuiPlatform`
or similar if it's in `vo-runtime` (this is a toolchain-level change, not a Studio change).

### 10.4 ext_builder.rs

New module for native extension compilation:

```rust
pub struct ExtBuilder {
    cache_root: PathBuf,       // ~/.vo/cache/ext/
    target_triple: String,
    rustc_version: String,
}

impl ExtBuilder {
    pub fn is_cached(&self, module: &str, version: &str) -> bool { ... }
    pub fn build(
        &self,
        module_root: &Path,
        module: &str,
        version: &str,
        on_line: impl Fn(&str),    // streaming build output
    ) -> Result<PathBuf, String> { ... }
    pub fn clean(&self, module: &str) -> Result<(), String> { ... }
}
```

Internally runs `cargo build --release` as a subprocess, streaming stdout/stderr
line by line to the caller.

---

## 11. WASM Backend Structure

### 11.1 wasm/src/lib.rs Cleanup

The current 730-line monolith is split:

```
studio/wasm/src/
  lib.rs              — WASM entry, re-exports
  compiler.rs         — compile_from_vfs, prepare_from_vfs
  runtime.rs          — GuestState, run_gui_entry, send_gui_event
  render_island.rs    — start_render_island, push/poll island data
  host_bridge.rs      — voHost* functions for toolchain WASM module
  shell_cache.rs      — shell handler bytecode caching (if we keep shell handler on WASM)
```

### 11.2 Shell Handler on WASM

Question: does the WASM backend still need the Vo shell handler program?

Current situation: the shell handler Vo program handles `fs.*`, `vo.*`, `git.*`, `zip.*`
operations on WASM by delegating to WASM versions of these tools.

New design options:

**Option A**: Keep the shell handler for operations that are naturally Vo programs (git, zip)
but handle fs/compiler operations directly in Rust/WASM.

**Option B**: Eliminate the shell handler entirely. Implement all operations in Rust/WASM
directly. Git operations use a WASM-compiled git library. Zip uses a WASM-compiled zip library.

I recommend **Option A**: keep the shell handler for git/zip (they're Vo modules that work
well in WASM already), but remove the fs/compiler routing through the shell handler.
The terminal's command parser directly calls `WasmBackend` methods.

---

## 12. Preview Panel Architecture

The preview panel is the rendering surface for GUI apps.

### 12.1 Component Structure

```
<PreviewPanel>
  ├── <PreviewToolbar>       — run/stop buttons, framework info
  └── <PreviewContent>
      ├── <ByteStreamRenderer>   — for render_byte_stream capability
      │   └── dynamically loaded framework renderer JS module
      ├── <SurfaceRenderer>      — for render_surface capability
      │   └── <canvas>
      └── <ConsoleRenderer>      — for console apps (just output)
```

### 12.2 Framework Renderer Loading

When a GUI app declares `render_byte_stream` + `renderer` in `vo.ext.toml`:

1. `RuntimeService.runGui()` returns `capabilities` including the renderer URL
2. `PreviewPanel` dynamically imports the renderer module
3. Renderer module is called with `init(host)` and `render(container, bytes)` on each frame
4. Renderer can register widgets via `host.registerWidget()`

For vogui: the existing `@vogui/index` + `@vogui/types` widget system continues to work,
but it's loaded as a renderer module — not hardcoded into Studio.

For voplay: the render-island widget is registered by voplay's renderer module.
Studio sees it as just another widget.

### 12.3 Render Flow

```
                      ┌─── Native ──────────────────────────────────┐
                      │                                              │
User Event ──→ PreviewPanel ──→ RuntimeService.sendEvent()          │
                      │              │                               │
                      │              ├── NativeBackend: Tauri IPC    │
                      │              │   → gui_runtime guest thread  │
                      │              │   → VM dispatches event       │
                      │              │   → returns render bytes      │
                      │              │                               │
                      │              └── WasmBackend: direct call    │
                      │                  → send_gui_event()          │
                      │                  → returns render bytes      │
                      │                                              │
Render bytes ←── PreviewPanel ←── renderer.render(container, bytes) │
                      └──────────────────────────────────────────────┘
```

---

## 13. URL / Launch Protocol

### 13.1 URL Scheme

```
# Dev mode (default)
https://studio.example.com/

# Dev mode with project from URL
https://studio.example.com/?project=https://github.com/user/repo/archive/main.tar.gz

# Dev mode with local path (native only)
studio --path /Users/alice/projects/my-vo-app

# Runner mode with URL
https://studio.example.com/?mode=runner&url=https://example.com/app.tar.gz

# Runner mode with local path (native only)
studio --mode=runner --path /Users/alice/projects/my-vo-app
```

### 13.2 Launch Sequence

```
1. Parse URL params / CLI args
2. Determine mode (dev / runner)
3. Determine backend (native / wasm)
4. Create Backend instance
5. Create all services
6. Resolve project source:
   a. URL → fetch, extract, set session root
   b. Local path → validate, set session root
   c. Workspace → use default workspace
7. Initialize services (workspace.init, project.open, etc.)
8. Mount UI (DevModeLayout or RunnerModeLayout)
9. If runner mode: auto-run the project entry point
```

---

## 14. Migration Path

This is a rewrite, not a refactor. But it can be done incrementally.

### Phase 1: Backend Interface + Service Skeleton

1. Define `Backend` interface in TypeScript
2. Implement `NativeBackend` wrapping existing Tauri commands (thin adapter)
3. Implement `WasmBackend` wrapping existing WASM exports (thin adapter)
4. Create service classes that delegate to `Backend`
5. Wire services into existing Svelte components

Result: same functionality, but services + backend abstraction in place.

### Phase 2: Terminal Rewrite

1. Add xterm.js dependency
2. Implement command parser
3. Implement `TerminalService` with command routing
4. Replace current "structured command" UI with xterm.js terminal
5. Keep all existing shell protocol operations working

### Phase 3: State Decomposition

1. Split `IdeState` into per-domain stores
2. Migrate components to use domain-specific stores
3. Remove the monolithic `ide` store

### Phase 4: Preview Panel + GUI Contract

1. Define `vo.ext.toml` `[studio]` section format
2. Implement framework renderer dynamic loading
3. Create generic `PreviewPanel` with capability-driven rendering
4. Migrate vogui renderer to a standalone JS module (loaded by Studio, not bundled)
5. Migrate voplay render-island to voplay's own renderer module
6. Remove all voplay-specific code from Studio

### Phase 5: Extension Build Pipeline

1. Implement `ext_builder.rs` in native backend
2. Implement `ExtensionService` with streaming build output
3. Wire `vo get` terminal command to extension service
4. Implement build cache with proper cache keys
5. Test full flow: `vo get`, build, load, run

### Phase 6: Tauri Backend Restructure

1. Split `lib.rs` into command modules
2. Clean `gui_runtime.rs` — remove voplay-specific code
3. Clean `AppState` — remove shell_runner, simplify session management
4. Remove old bridge.ts and shell handler routing

### Phase 7: Runner Mode

1. Implement `RunnerModeLayout` component
2. Implement mode detection from URL/CLI args
3. Auto-run logic for runner mode
4. Test both dev and runner modes

---

## 15. Resolved Questions

1. **vogui JS renderer packaging** — vogui's JS renderer lives in `vogui/js/` (the vogui
   repo), not in Studio. Studio dynamically loads it via the GUI Framework Contract
   (Section 8). No packaging change needed.

2. **Multi-session** — Not planned. Single session per Studio instance.

3. **Collaboration** — Not planned.

4. **Hot reload** — Desirable but deferred. Design space reserved: `WorkspaceService.watch()`
   → `CompilerService.check()` on file change → optional auto-recompile → `RuntimeService`
   restart. Implementation is not part of the initial rewrite scope.

5. **Debugger** — Not planned.
