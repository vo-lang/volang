# Studio Codebase Review — 2026-03-04

## Executive Summary

A comprehensive review of the entire `studio/` codebase covering architecture, frontend
(Svelte components, stores, lib), backend (Tauri, WASM bridge), Vo shell handlers, and
build/config files. The codebase is functional and well-structured at the top level, but
has accumulated architectural debt: dual FS APIs, a broken unified-interface promise on
the WASM side, God-module anti-patterns, dead code, and a copy-paste bug in streaming.

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│  Frontend (Svelte)                                          │
│  App.svelte → Sidebar, Toolbar, Editor, FileTree,           │
│               Home, Console, Terminal, PreviewPanel,         │
│               ContextMenu, SyncDetailModal                  │
│                                                             │
│  Stores: ide, explorer, terminal, github, projects          │
│  Lib:    actions.ts, bridge.ts, terminal_cmd.ts             │
│          shell/ (protocol, client, transport, wasm/*)       │
├─────────────────────────────────────────────────────────────┤
│  Bridge Interface (bridge.ts)                               │
│  ├── Direct FS methods: fsListDir, fsReadFile, fsWriteFile… │
│  └── Shell API: ShellClient (protocol + transport)          │
├────────────────────┬────────────────────────────────────────┤
│  Tauri Backend     │  WASM Backend                          │
│  src-tauri/        │  wasm/src/lib.rs                       │
│  ├── lib.rs        │  ├── WasmShellRouter                   │
│  ├── shell/        │  │   ├── WasmVoHandler (vo.* ops)      │
│  │   ├── mod.rs    │  │   └── WasmVoRunner  (fs/git/zip/…)  │
│  │   ├── router.rs │  └── WasmFsHandler, WasmGitHandler,    │
│  │   └── vo_runner │      WasmProcHandler, WasmZipHandler    │
│  │       .rs       │      (DEAD CODE — unused)              │
├────────────────────┴────────────────────────────────────────┤
│  Vo Shell Handlers (vo/shell/)                              │
│  main.vo → fs.vo, git.vo, proc.vo, vo_ops.vo, zip.vo,      │
│            http.vo                                          │
│  Runs on both Tauri (native) and WASM (embedded sources)    │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. Findings

### 2.1 Architecture-Level Issues

#### A1. Dual FS API — Bridge exposes both direct FS and ShellClient

**Location**: `src/lib/bridge.ts`

The `Bridge` interface exposes two parallel sets of filesystem operations:

1. **Direct methods**: `fsListDir()`, `fsReadFile()`, `fsWriteFile()`, `fsMkdir()`,
   `fsRename()`, `fsRemove()` — called by `actions.ts`, `Home.svelte`, `projects.ts`
2. **ShellClient**: `bridge.shell.exec({ kind: 'fs.read', ... })` — called by
   `terminal_cmd.ts`

On the Tauri side this means **two independent FS implementations**:
- `lib.rs` → `cmd_fs_list`, `cmd_fs_read`, etc. (direct Tauri commands)
- `vo/shell/fs.vo` (via VoRunner)

**Impact**: Code duplication, potential behavior divergence, increased maintenance burden.

**Recommendation**: Deprecate and remove direct FS methods from `Bridge`. All FS
operations should go through `ShellClient`. This also removes the Tauri-side duplicate
`cmd_fs_*` commands.

#### A2. WASM side breaks unified interface — `vo.*` ops bypass Vo handler

**Location**: `src/lib/shell/wasm/router.ts:60-66`

On WASM, `vo.*` ops are routed to `WasmVoHandler` (TypeScript), which calls
`compileRunEntry` directly. They do **not** go through `WasmVoRunner` → `vo/shell/vo_ops.vo`.

This means:
- `vo_ops.vo` logic (diagnostics parsing, path resolution, sandbox checks) is
  re-implemented in TypeScript (`vo_handler.ts`)
- Two separate diagnostic parsers exist: `parseDiagLine()` in `vo_ops.vo` (Vo) and
  `parseDiagLine()` in `vo_handler.ts` (TS)
- Behavioral differences between Tauri and WASM are likely

**Recommendation**: Expose `libs/vox` FFI functions to the WASM runtime so `vo_ops.vo`
can run in WASM directly via `WasmVoRunner`. Eliminate `WasmVoHandler` entirely.

#### A3. Tauri ShellRouter is an empty wrapper

**Location**: `src-tauri/src/shell/router.rs`

`ShellRouter` contains zero routing logic — it delegates everything to `VoRunner`:

```rust
pub fn handle(&self, req: ShellRequest, app: &tauri::AppHandle) -> ShellResponse {
    self.runner.handle(req, app)
}
```

Compare with WASM's `WasmShellRouter` which actually routes `vo.*` vs other ops.

**Recommendation**: Either remove the wrapper (use `VoRunner` directly) or give it
real routing responsibility (e.g., some ops handled natively in Rust for performance).

#### A4. VoRunner recompiles shell handler on every request

**Location**: `src-tauri/src/shell/vo_runner.rs:68-71`

```rust
let compiled = compile(&handler_path)
    .map_err(|e| ShellError::internal(...))?;
```

Every `ls`, `cat`, `stat` operation triggers a full Vo compilation of the handler.

**Recommendation**: Cache the compiled bytecode. Shell handler code is static after
build. Use `OnceCell<CompiledOutput>` or `Mutex<Option<CompiledOutput>>` with a
file-mtime check for dev mode.

#### A5. Global output capture lock forces serial execution

**Location**: `src-tauri/src/shell/vo_runner.rs:23`

```rust
static VO_EXEC_LOCK: Mutex<()> = Mutex::new(());
```

`output::start_capture()` / `stop_capture()` is process-global state. All shell
operations are serialized behind this lock.

**Impact**: No concurrent shell ops possible. Acceptable for now but is an
architectural ceiling.

**Recommendation**: Long-term: change `vo-runtime` output capture to per-execution
sinks. Short-term: document this limitation.

#### A6. ShellRouter recreated per request

**Location**: `src-tauri/src/shell/mod.rs:134-140`

```rust
pub fn cmd_shell_exec(...) -> Result<ShellResponse, String> {
    let router = ShellRouter::new(state.workspace_root.clone());
    Ok(router.handle(req, &app))
}
```

A new `ShellRouter` (and `VoRunner`) is created for every shell request. This prevents
caching compiled handlers.

**Recommendation**: Move `ShellRouter` into `AppState` as a persistent instance.

---

### 2.2 Vo Shell Handler Issues (`vo/shell/`)

#### S1. Dispatch in `main.vo` uses inconsistent routing style

**Location**: `vo/shell/main.vo:51-79`

`fs.*`, `git.*`, `zip.*` use exhaustive string equality chains:
```vo
case kind == "fs.list" || kind == "fs.stat" || kind == "fs.read" || ...
```

While `vo.*` and `http.*` use prefix matching:
```vo
case strings.HasPrefix(kind, "vo."):
case strings.HasPrefix(kind, "http."):
```

**Impact**: Adding a new op (e.g., `fs.watch`) requires updating both `main.vo`
dispatch AND the sub-handler file. Prefix routing only needs the sub-handler.

**Recommendation**: Use `strings.HasPrefix` for all domain groups:
```vo
case strings.HasPrefix(kind, "fs."):
    return handleFs(id, workspace, cwd, kind, opRaw)
case strings.HasPrefix(kind, "git."):
    return handleGit(id, workspace, cwd, kind, opRaw)
case kind == "proc.spawn":
    return handleProc(id, workspace, cwd, opRaw)
case strings.HasPrefix(kind, "vo."):
    return handleVoOp(id, workspace, cwd, kind, opRaw)
case strings.HasPrefix(kind, "zip."):
    return handleZip(id, workspace, cwd, opRaw)
case strings.HasPrefix(kind, "http."):
    return handleHttp(id, cwd, kind, opRaw)
```

#### S2. Excessive boilerplate in `fs.vo` — path resolution repeated 10+ times

**Location**: `vo/shell/fs.vo` — every handler function

Every function repeats:
```vo
var path string
path = op~>path?
abs, err := resolveAndSandbox(workspace, cwd, path)
if err != nil {
    writeError(id, "ERR_ACCESS_DENIED", err.Error())
    return nil
}
```

**Recommendation**: Extract a helper:
```vo
func resolvePathParam(id, workspace, cwd string, op any, key string) (string, bool) {
    var path string
    path = op~>[key]?
    abs, err := resolveAndSandbox(workspace, cwd, path)
    if err != nil {
        writeError(id, "ERR_ACCESS_DENIED", err.Error())
        return "", false
    }
    return abs, true
}
```

#### S3. `http.vo` handler signature inconsistent — missing `workspace` parameter

**Location**: `vo/shell/http.vo:9`

```vo
func handleHttp(id, cwd string, kind string, op any) error {
```

All other handlers: `(id, workspace, cwd, kind, op)`. Missing `workspace` makes the
dispatch in `main.vo` also inconsistent (line 74 doesn't pass `workspace`).

**Recommendation**: Add `workspace` parameter for consistency, even if unused.

#### S4. Streaming ops are WASM-unaware — Vo handler returns `stream` response that WASM silently rejects

**Location**: `git.vo:85-147` (clone/push/pull), `proc.vo` (spawn)

The Vo handler constructs `writeStream(...)` responses. On WASM, `WasmVoRunner` receives
this and converts it to `ERR_NOT_SUPPORTED` (vo_runner.ts:78-86).

**Problem**: The handler doesn't know it's running on WASM. It does unnecessary work
building a streamCmd that will be discarded. The error message is generic
("streaming shell ops are not supported") rather than contextual
("git push requires the desktop app").

**Recommendation**: Pass platform info (`args[0]` is already `'wasm'` on WASM). Let the
handler check platform and return a specific error message at the Vo level:
```vo
if platform == "wasm" {
    writeError(id, "ERR_NOT_SUPPORTED", "git push requires the desktop app")
    return nil
}
```

#### S5. `vo_ops.vo` registers ops that always return `ERR_NOT_SUPPORTED`

**Location**: `vo/shell/vo_ops.vo:294-310`

`vo.get`, `vo.test`, `vo.bench` are registered but always return errors.

**Recommendation**: Move these "not available in Studio" messages to the frontend
(`terminal_cmd.ts`) to avoid a full JSON → Vo compile → execute → JSON round-trip
just to return an error. Or remove them entirely — the `default` case in `handleVoOp`
already returns `ERR_NOT_SUPPORTED`.

#### S6. `fs.vo` copy is file-only — no recursive directory copy

**Location**: `vo/shell/fs.vo:216-244`

`handleFsCopy` reads a single file and writes it. Copying a directory will fail silently
(read error on a directory path).

**Recommendation**: Either implement recursive copy or return a clear error when the
source is a directory.

---

### 2.3 Frontend Issues

#### F1. `actions.ts` is a 754-line God Module

**Location**: `src/lib/actions.ts`

Handles: workspace init, file CRUD, directory management, project management (push/pull/
rename/delete), code execution, editor state sync — all in one file.

**Recommendation**: Split by domain:
- `actions/workspace.ts` — init, loadDir, toggleDir
- `actions/file.ts` — openFile, saveFile, createEntry, deleteEntry, renameEntry
- `actions/project.ts` — pushProject, pullProject, openProjectEntry, deleteProject
- `actions/execution.ts` — runCode, stopCode, onEditorChange

#### F2. `terminal_cmd.ts` is 998 lines mixing parsing, dispatch, and formatting

**Location**: `src/lib/terminal_cmd.ts`

Three distinct concerns in one file:
1. Command parsing (tokenize, path resolve, arg parsing)
2. Command dispatch (construct ShellOp, call shell.exec/stream)
3. Output formatting (formatLs, formatGitStatus, formatGitLog, ANSI codes)

**Recommendation**: Extract `terminal_fmt.ts` for all output formatting functions.

#### F3. Stores contain heavy business logic

**Location**: `src/stores/github.ts` (426 lines), `src/stores/projects.ts` (346 lines)

These stores include API call logic (`githubFetch`, `createGist`, `updateGist`,
`discoverLocalProjects`, `mergeProjects`, `hashFiles`, etc.) mixed with state
definitions.

**Recommendation**: Move API/business logic to service modules. Stores should only
contain state definitions and simple derived helpers.

#### F4. `Home.svelte` is 1064 lines with 5 embedded modals

**Location**: `src/components/Home.svelte`

Contains: auth modal, project card grid, context menu, rename modal, sync detail modal
entry — all in a single component.

**Recommendation**: Extract `AuthModal.svelte`, `ProjectCard.svelte`,
`ProjectContextMenu.svelte` as child components.

#### F5. Terminal prompt cwd not snapshotted per line

**Location**: `src/components/Terminal.svelte:305`

All `input`-type lines render using current `cwd`. After `cd`, historical prompts
retroactively change their displayed path.

**Recommendation**: Record `cwd` in `TermLine` at creation time.

#### F6. `expandedDirs` uses `string[]` with `includes()` — O(n) lookups

**Location**: `src/stores/ide.ts`, `src/components/FileTree.svelte:47`

```ts
if (entry.isDir && expandedDirs.includes(entry.path)) {
```

**Recommendation**: Use `Set<string>` for O(1) lookups.

---

### 2.4 Bugs

#### B1. `combined` flag is a no-op — copy-paste bug

**Location**: `src-tauri/src/shell/vo_runner.rs:164-168`

```rust
if combined {
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
} else {
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
}
```

Both branches are identical. `combined=true` should merge stderr into stdout
(e.g., redirect stderr to stdout handle).

**Priority**: P0 — functional bug.

#### B2. WASM capabilities declaration is inaccurate

**Location**: `src/lib/shell/wasm/router.ts:43-57`

Declares `git`, `zip`, `proc.spawn` as capabilities, but most of these ops return
`ERR_NOT_SUPPORTED` in practice.

**Recommendation**: Capabilities should reflect actual WASM support. Either remove
unsupported capabilities or add granularity (`git.read` vs `git.write`).

---

### 2.5 Dead Code & Cleanup

#### D1. Deprecated components not deleted

- `src/components/FileExplorer.svelte` — contains only a deprecation comment
- `src/components/GistSaveDialog.svelte` — contains only a deprecation comment

**Action**: Delete both files.

#### D2. Unused WASM handler classes

- `src/lib/shell/wasm/git_handler.ts` — not imported by router
- `src/lib/shell/wasm/zip_handler.ts` — not imported by router
- `src/lib/shell/wasm/proc_handler.ts` — constructor still used? (router imports type)
- `src/lib/shell/wasm/fs_handler.ts` — `VfsLike` type imported but class unused

These are pre-WasmVoRunner removed code. `WasmShellRouter` routes all non-vo ops to
`WasmVoRunner`, not to these handlers.

**Action**: Delete unused handler classes. Keep `VfsLike` type if still imported.

#### D3. Unused `zip` crate in Tauri Cargo.toml

**Location**: `src-tauri/Cargo.toml:25`

```toml
zip = "2"
```

Zip operations are handled by the Vo shell handler (`zip.vo` using `3rdparty/zip`).
The Rust `zip` crate is not used in any Tauri source file.

**Action**: Remove from dependencies.

---

### 2.6 Build & Config Issues

#### C1. Monaco editor hardcoded to Go language

**Location**: `src/components/Editor.svelte:17`

```ts
language: 'go',
```

Vo is not Go. Monaco's Go language service will flag Vo-specific syntax (`?`,
`errdefer`, `~>`, `fail`) as errors.

**Recommendation**: Register a custom Vo language definition for Monaco, or at minimum
use `plaintext` to avoid false error squiggles.

#### C2. Studio CSS depends on vogui's postcss config — implicit coupling

**Location**: `vite.config.ts:27`

```ts
postcss: resolve(__dirname, '../libs/vogui/js'),
```

If vogui's postcss config changes, Studio's CSS build breaks silently.

**Recommendation**: Either copy the required postcss config into Studio or make the
dependency explicit via a shared config package.

---

## 3. Implementation Log

### 2026-03-04: git2 WASM stubs (DONE)

**Problem**: When `run_shell_handler` ran the Vo shell handler bytecode in WASM and
a `git.*` op was invoked, the bytecode called `nativeOpen` / `nativeStatus` / etc.
(git2 extern functions). These were never registered in the WASM VM — the `create_vm`
call used `|_, _| {}` (no externs). An unregistered extern causes the VM to panic,
surfacing as `ERR_INTERNAL` or a WASM thread crash, not a clean `ERR_NOT_SUPPORTED`.

**Why isomorphic-git is not the answer**:
- isomorphic-git API is fully async (Promise-based); Vo externs are synchronous
- The WASM VirtualFS has no `.git` directory — git read ops return empty data anyway
- Git write/network ops require process spawning or HTTP backends, both unavailable
- The WASM Studio "git sync" story is the GitHub REST API integration in `Home.svelte`

**Decision**: Register WASM stub externs for all 15 `native*` functions in git2.vo.
Stubs return a descriptive error message pointing users to the desktop app.
Follow the same pattern as `runtime-wasm/src/exec.rs` (os/exec stubs).

**Files changed**:

1. **`3rdparty/git2/wasm_stubs.rs`** (NEW) — standalone Rust source included via
   `include!` into the studio WASM crate. Contains 15 stub functions grouped by
   return type (`stub_uint_err`, `stub_bytes_err`, `stub_bool_err`, `stub_err_only`)
   and `pub fn register_externs(registry, externs)` that matches on the normalized
   extern names (`github_com_vo_lang_git2_nativeXxx`).

2. **`studio/wasm/src/lib.rs`** — added `mod git2_stubs` block with `include!`
   (path resolved via `CARGO_MANIFEST_DIR`). Changed `run_shell_handler`'s
   `create_vm(&bytecode, |_, _| {})` → `create_vm(&bytecode, git2_stubs::register_externs)`.
   No new Cargo dependency needed — the stubs only use `vo_runtime` which is already
   in `studio/wasm/Cargo.toml`, and the git2 C library is NOT compiled for WASM
   (the stubs file is pure Rust using only `vo_runtime::ffi` types).

3. **`studio/src/lib/shell/wasm/router.ts`** — removed `'git'`, `'zip'`,
   `'proc.spawn'` from `capabilities()` since they all return `ERR_NOT_SUPPORTED`
   in WASM. Updated the architecture comment to document exact per-domain status.

**Why NOT add vo-git2 as a WASM Cargo dependency**: `3rdparty/git2/rust/Cargo.toml`
has `git2 = { features = ["vendored-libgit2", "vendored-openssl"] }` as a
**non-optional** dependency. Adding this crate to `studio/wasm` would attempt to
compile libgit2 + OpenSSL for WASM, which fails. The `include!` approach compiles
only the stubs code (pure Rust, no C libs) as part of the studio WASM crate.

---

## 4. Improvement Plan

### Phase 1: Critical Fixes (1-2 days)

| # | Task | Files | Priority |
|---|------|-------|----------|
| 1 | Fix `combined` flag copy-paste bug | `src-tauri/src/shell/vo_runner.rs` | P0 |
| 2 | Delete deprecated components | `FileExplorer.svelte`, `GistSaveDialog.svelte` | P0 |
| 3 | Delete unused WASM handler classes | `git_handler.ts`, `zip_handler.ts`, (check `proc_handler.ts`, `fs_handler.ts` imports) | P0 |
| 4 | Remove unused `zip` crate | `src-tauri/Cargo.toml` | P0 |

### Phase 2: Unify Shell Interface (3-5 days)

| # | Task | Files | Priority |
|---|------|-------|----------|
| 5 | Use `HasPrefix` for all dispatch groups in `main.vo` | `vo/shell/main.vo` | P1 |
| 6 | Fix `http.vo` handler signature (add `workspace`) | `vo/shell/http.vo`, `vo/shell/main.vo` | P1 |
| 7 | Extract `resolvePathParam` helper in `fs.vo` | `vo/shell/fs.vo` | P1 |
| 8 | Move `ShellRouter` into `AppState` (persistent) | `src-tauri/src/shell/mod.rs`, `src-tauri/src/lib.rs` | P1 |
| 9 | Cache compiled Vo handler bytecode in `VoRunner` | `src-tauri/src/shell/vo_runner.rs` | P1 |
| 10 | ~~Fix WASM capabilities~~ **DONE** — git2 stubs + accurate capabilities | `3rdparty/git2/wasm_stubs.rs`, `studio/wasm/src/lib.rs`, `wasm/router.ts` | P1 ✓ |

### Phase 3: Eliminate Dual APIs (5-7 days)

| # | Task | Files | Priority |
|---|------|-------|----------|
| 11 | Expose `libs/vox` FFI to WASM runtime | `studio/wasm/src/lib.rs`, `vo-web/src/lib.rs` | P1 |
| 12 | Route WASM `vo.*` ops through `WasmVoRunner` → `vo_ops.vo` | `src/lib/shell/wasm/router.ts` | P1 |
| 13 | Delete `WasmVoHandler` after #12 | `src/lib/shell/wasm/vo_handler.ts`, `router.ts` | P1 |
| 14 | Deprecate direct FS methods on Bridge | `src/lib/bridge.ts` | P1 |
| 15 | Migrate `actions.ts`, `Home.svelte`, `projects.ts` to use ShellClient for FS | Multiple frontend files | P1 |
| 16 | Remove `cmd_fs_*` commands from Tauri backend | `src-tauri/src/lib.rs` | P1 |

### Phase 4: Frontend Refactoring (3-5 days)

| # | Task | Files | Priority |
|---|------|-------|----------|
| 17 | Split `actions.ts` into domain modules | `src/lib/actions/` | P2 |
| 18 | Extract `terminal_fmt.ts` from `terminal_cmd.ts` | `src/lib/terminal_fmt.ts` | P2 |
| 19 | Move business logic out of `github.ts` and `projects.ts` stores | `src/stores/`, `src/lib/services/` | P2 |
| 20 | Split `Home.svelte` into sub-components | `src/components/` | P2 |
| 21 | Snapshot cwd per terminal line | `src/stores/terminal.ts`, `Terminal.svelte` | P2 |
| 22 | Change `expandedDirs` to `Set<string>` | `src/stores/ide.ts`, `FileTree.svelte` | P2 |

### Phase 5: Platform Awareness & Polish (2-3 days)

| # | Task | Files | Priority |
|---|------|-------|----------|
| 23 | Add platform detection to Vo shell handlers | `vo/shell/main.vo`, `git.vo`, `proc.vo` | P2 |
| 24 | Move "not available in Studio" messages to frontend | `terminal_cmd.ts`, `vo_ops.vo` | P3 |
| 25 | Register Vo language definition for Monaco | `src/components/Editor.svelte` | P3 |
| 26 | Remove or simplify empty `ShellRouter` wrapper | `src-tauri/src/shell/router.rs` | P3 |
| 27 | Decouple Studio postcss from vogui | `vite.config.ts` | P3 |

---

## 4. Unified Interface Scorecard

| Dimension | Current | Target |
|-----------|---------|--------|
| Tauri: all ops via Vo handler | ★★★★★ | ★★★★★ |
| WASM: all ops via Vo handler | ★★☆☆☆ | ★★★★★ (after Phase 3) |
| Bridge: single API surface | ★★☆☆☆ | ★★★★★ (after Phase 3) |
| Handler code quality | ★★★☆☆ | ★★★★☆ (after Phase 2) |
| Platform awareness | ★★☆☆☆ | ★★★★☆ (after Phase 5) |
| Frontend modularity | ★★☆☆☆ | ★★★★☆ (after Phase 4) |

---

## 5. Files Reviewed

### Frontend
- `src/App.svelte` — root layout, mode switching
- `src/components/Sidebar.svelte` — navigation sidebar
- `src/components/Toolbar.svelte` — run/stop buttons, file info
- `src/components/Editor.svelte` — Monaco editor wrapper
- `src/components/FileTree.svelte` — file explorer tree
- `src/components/Terminal.svelte` — terminal UI, ANSI parsing, tab completion
- `src/components/Console.svelte` — output console panel
- `src/components/PreviewPanel.svelte` — GUI preview panel
- `src/components/Home.svelte` — project management home screen
- `src/components/ContextMenu.svelte` — file tree context menu
- `src/components/SyncDetailModal.svelte` — sync diff modal
- `src/components/OutputPanel.svelte` — simple output panel
- `src/components/FileExplorer.svelte` — DEPRECATED
- `src/components/GistSaveDialog.svelte` — DEPRECATED

### Lib
- `src/lib/bridge.ts` — Bridge interface + Tauri/WASM implementations
- `src/lib/actions.ts` — all business logic actions
- `src/lib/terminal_cmd.ts` — terminal command interpreter

### Shell
- `src/lib/shell/protocol.ts` — shell protocol types
- `src/lib/shell/client.ts` — ShellClient
- `src/lib/shell/wasm/router.ts` — WASM shell router
- `src/lib/shell/wasm/vo_handler.ts` — WASM Vo toolchain handler
- `src/lib/shell/wasm/vo_runner.ts` — WASM Vo shell runner
- `src/lib/shell/wasm/fs_handler.ts` — WASM FS handler (unused)
- `src/lib/shell/wasm/git_handler.ts` — WASM Git handler (unused)
- `src/lib/shell/wasm/proc_handler.ts` — WASM Proc handler (unused by router)
- `src/lib/shell/wasm/zip_handler.ts` — WASM Zip handler (unused)

### Stores
- `src/stores/ide.ts` — IDE state
- `src/stores/explorer.ts` — explorer/sidebar state
- `src/stores/terminal.ts` — terminal state
- `src/stores/github.ts` — GitHub integration
- `src/stores/projects.ts` — project management

### Backend (Tauri)
- `src-tauri/src/lib.rs` — Tauri commands
- `src-tauri/src/shell/mod.rs` — shell types + Tauri commands
- `src-tauri/src/shell/router.rs` — shell router (thin wrapper)
- `src-tauri/src/shell/vo_runner.rs` — Vo handler runner

### Vo Shell Handlers
- `vo/shell/main.vo` — entry + dispatch
- `vo/shell/fs.vo` — filesystem ops
- `vo/shell/git.vo` — git ops (libgit2 reads + CLI writes)
- `vo/shell/proc.vo` — process spawning
- `vo/shell/vo_ops.vo` — Vo toolchain ops
- `vo/shell/zip.vo` — zip archive ops
- `vo/shell/http.vo` — HTTP request ops

### Config
- `vite.config.ts` — Vite build config
- `svelte.config.ts` — Svelte preprocessor config
- `tsconfig.json` — TypeScript config
- `src-tauri/Cargo.toml` — Rust dependencies
