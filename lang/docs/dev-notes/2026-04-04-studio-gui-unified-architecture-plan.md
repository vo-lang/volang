# Studio GUI Unified Architecture Plan

**Date**: 2026-04-04
**Status**: Proposed

## Core Principles

1. **Web and Native GUI rendering paths are fully unified.** Both run the
   GUI inside the webview WASM VM (`vo-studio-wasm`). The only distinction
   Native has is the ability to spawn a **compute island** that runs native
   code in a separate thread.
2. **Web cannot compile Rust.** WASM extension artifacts (`.wasm` + JS glue)
   must be precompiled for the web platform. Native can build them locally
   via `cargo build` / `wasm-pack` during development.

## Current Architecture (as of 2026-04-04)

### Two divergent `runGui` flows

| Step | Web (`WebBackend`) | Native (`NativeBackend`) |
|------|--------------------|--------------------------|
| Compile Vo | `vo-studio-wasm` internal (`vo-web` compiler, runs in browser WASM) | `cmd_compile_gui` (`vo-engine`, Rust native process) |
| Load WASM exts | `vo-studio-wasm` internal (bundled in `runGui`) | Frontend loop: `preloadExtModule()` per ext |
| Build WASM exts | Impossible | `ensure_standalone_wasm_ext_fresh` / `ensure_pkg_island_fresh` |
| Host bridge preload | Not done before initial render | Loaded before `runGuiFromBytecode` |
| GUI execution | `wasm.runGui(path)` (compile + run, one call) | `wasm.runGuiFromBytecode(bytecode)` |
| Standalone dispatcher | ✅ `installStandaloneGuiDispatcher` | ✅ `installStandaloneGuiDispatcher` |
| VFS snapshot | `wasm.getRenderIslandVfsSnapshot()` (in-memory) | `cmd_get_render_island_vfs_snapshot` (Tauri IPC, filesystem walk) |
| Protocol module | Loaded in `PreviewPanel.launchRenderIsland()` | Loaded in `PreviewPanel.launchRenderIsland()` |
| Host bridge (render island) | Loaded in `PreviewPanel.launchRenderIsland()` | Loaded in `PreviewPanel.launchRenderIsland()` (second load, cached) |

### Key files

- `studio/src/lib/backend/native_backend.ts` — Native Tauri backend
- `studio/src/lib/backend/web_backend.ts` — Web (browser-only) backend
- `studio/src/lib/backend/backend.ts` — Platform-agnostic `Backend` interface
- `studio/src-tauri/src/commands/gui.rs` — Rust backend commands
- `studio/src/lib/gui/render_island.ts` — Render island module loading
- `studio/src/components/PreviewPanel.svelte` — GUI preview UI component
- `studio/src/lib/studio_wasm.ts` — WASM VM singleton and ext loading
- `studio/src/lib/services/runtime_service.ts` — GUI session lifecycle
- `studio/wasm/src/lib.rs` — `vo-studio-wasm` Rust source

## Identified Issues

### Issue 1: Two divergent `runGui` pipelines

`NativeBackend.runGui()` does: compile (Rust) → load WASM VM → preload
exts → install dispatcher → load host bridge → `runGuiFromBytecode`.

`WebBackend.runGui()` does: load WASM VM → `prepareEntry` → install
dispatcher → `wasm.runGui(path)` (compile + ext load + run, all internal).

All GUI lifecycle concerns (session guards, host bridge timing, protocol
module loading, hostWidgetHandlerId extraction) must be implemented and
maintained in two places. The behavior has already started diverging.

### Issue 2: Host bridge loaded twice on Native, zero times before initial render on Web

`NativeBackend.runGui()` loads the host bridge before `runGuiFromBytecode`
so that `host_measure_text` returns real values during the first render.
`PreviewPanel.launchRenderIsland()` loads it again (cached early return).

On Web, `wasm.runGui()` executes compile + run in one call. The host bridge
is only loaded later in `PreviewPanel.launchRenderIsland()`. This means the
initial render on Web has **no host bridge** — `host_measure_text` will
throw or return zero, causing the same VirtualScroll regression that was
fixed on Native.

### Issue 3: `cmd_run_gui` and related Tauri commands are dead code

The following commands are registered in `lib.rs` but **never called** from
the frontend:

- `cmd_run_gui` — old pure-native VM GUI path
- `cmd_send_gui_event` / `cmd_send_gui_event_async`
- `cmd_poll_gui_render`
- `cmd_stop_gui`
- `__island_transport_push`

Associated `AppState` guest-handle management and `gui_runtime::run_gui`
are also dead.

### Issue 4: VFS snapshot fetched 3 times per GUI launch

On Native, each of `loadHostBridgeModule`, `loadProtocolModule`, and
`loadRendererAndSnapshot` independently calls
`cmd_get_render_island_vfs_snapshot` via Tauri IPC. Each call does a full
filesystem walk. On Web, same 3 calls but via in-memory WASM (cheaper but
still redundant).

### Issue 5: `hostWidgetHandlerId` lost on initial frame (Native WASM path)

`NativeBackend.runGui()` returns `hostWidgetHandlerId: null`.
`RuntimeService` tries to recover via `protocolModule`, but protocol module
is only loaded in `PreviewPanel.launchRenderIsland()` — after `runGui`
returns. The old `cmd_run_gui` Rust path correctly called
`find_on_widget_handler_id` before returning.

On Web, `wasm.runGui()` returns the handler id from the Rust side, so it
works correctly.

### Issue 6: WASM ext build logic buried inside ext spec reading

`ensure_standalone_wasm_ext_fresh` is called from `read_wasm_ext_spec` →
`extract_wasm_ext_specs`, deeply nested inside `cmd_compile_gui`. The
`cargo build` side-effect is hidden in what looks like a read operation.
Similarly, `ensure_pkg_island_fresh` is called from
`cmd_get_render_island_vfs_snapshot`.

## Target Architecture

```
              ┌──────────────────────────────────────┐
              │        Shared Frontend Layer          │
              │  RuntimeService + PreviewPanel +      │
              │  render_island.ts + studio_wasm.ts    │
              └──────────────┬───────────────────────┘
                             │
                    Backend.runGui(path)
                             │
              ┌──────────────┼──────────────┐
              │                             │
        NativeBackend                 WebBackend
              │                             │
     ┌────────┴────────┐          ┌─────────┴──────────────┐
     │  cmd_compile_gui │          │  wasm.compileGui(path) │
     │  (vo-engine,     │          │  (vo-web compiler,     │
     │   native proc)   │          │   runs in browser)     │
     └────────┬────────┘          └─────────┬──────────────┘
              │                             │
              └──────────┬──────────────────┘
                         │
              { bytecode, framework, extSpecs }
                         │
              ┌──────────┴──────────────────┐
              │   Shared post-compile flow   │
              │   1. preloadExtModules()     │
              │   2. loadHostBridge()        │
              │   3. runGuiFromBytecode()    │
              └─────────────────────────────┘
                         │
              ┌──────────┴──────────────────┐
              │   PreviewPanel (shared)      │
              │   1. loadProtocolModule()    │
              │   2. startRenderIsland()     │
              │   3. deliverRenderBytes()    │
              └─────────────────────────────┘

Native-only sidecar:
  ┌─────────────────────────────────────┐
  │  Compute Island (native thread)     │
  │  - Runs native Rust code            │
  │  - Communicates via island channel  │
  │  - cargo build for local dev exts   │
  └─────────────────────────────────────┘
```

### Design decisions

1. **Both backends produce the same intermediate output**: `{ bytecode,
   framework, extSpecs }`. Native gets it from `cmd_compile_gui` (Rust
   process). Web gets it from a new `wasm.compileGui(path)` export that
   compiles but does not execute.

2. **Post-compile flow is shared code**, not duplicated per backend. A
   shared function takes `{ bytecode, framework, extSpecs }` and runs the
   sequence: preload WASM exts → load host bridge → `runGuiFromBytecode`.

3. **WASM ext build is a separate native-only prepare step**, not embedded
   in compile or VFS snapshot. For published modules, `.wasm` artifacts
   already exist. For local dev modules (`vo.work` replace), native calls
   `cargo build` / `wasm-pack` before compile. Web skips this (uses
   precompiled artifacts).

4. **VFS snapshot is fetched once and shared** across protocol module,
   host bridge module, and renderer module loading.

5. **Protocol module ownership**: the shared post-compile function is
   responsible for loading the protocol module and calling
   `runtime.setProtocolModule(protocol)` so that `hostWidgetHandlerId`
   is available immediately from the first render frame. `PreviewPanel`
   retains responsibility for **teardown only** (`unloadProtocolModule` +
   `runtime.setProtocolModule(null)`). The module loaders (`loadProtocolModule`,
   `loadHostBridgeModule`, `loadRendererAndSnapshot`) must be **path-aware**
   and **session-keyed** so that different frameworks or reloads never
   reuse a stale cached module.

## Development Plan

Phases ordered from lowest to highest risk and dependency:

### Phase 1: Remove debug logging

**Goal**: Remove temporary diagnostic logging added during development.

**Files**:
- `studio/src/lib/backend/native_backend.ts` — Remove `cmd_debug_log`
  calls in `runGui()` (bridge-diag messages).
- `studio/src/lib/gui/render_island.ts` — Remove `console.log` in
  `loadHostBridgeModule`.

**Validation**: `vite build` in `studio/` passes clean.

### Phase 2: Remove dead native VM GUI path

**Goal**: Delete the old pure-native VM GUI path that is no longer called
from the frontend. All GUI now runs through the webview WASM VM.

**Files**:
- `studio/src-tauri/src/commands/gui.rs` — Delete `cmd_run_gui`,
  `cmd_send_gui_event`, `cmd_send_gui_event_async`, `cmd_poll_gui_render`,
  `cmd_stop_gui`, `__island_transport_push`, `find_on_widget_handler_id`.
- `studio/src-tauri/src/lib.rs` — Remove those commands from
  `generate_handler![]`.
- `studio/src-tauri/src/state.rs` — Remove `guest: Mutex<Option<GuestHandle>>`,
  `push_rx: Mutex<Option<Arc<SyncRenderBuffer>>>`, `install_guest_runtime`,
  `clear_guest_runtime`, `with_guest`, `poll_gui_render`. Remove
  `use crate::gui_runtime::GuestHandle` import.
- `studio/src-tauri/src/gui_runtime.rs` — Delete `run_gui` function and
  all `vo_app_runtime::spawn_native_gui` / `NativeGuiEventLoopConfig`
  usage. Keep `StudioLogRecord`, `emit_studio_log`, `make_studio_log_sink`,
  `debug_log` (all still used by `cmd_compile_gui` and `cmd_debug_log`).

**Note**: `cmd_compile_gui` still calls `state.clear_guest_runtime()` at
the top — remove that call when deleting `clear_guest_runtime` from state.

**Validation**: `cargo check` in `studio/src-tauri`. No frontend
references to removed commands (verify with grep).

### Phase 3: Make module loaders path-aware and session-keyed

**Goal**: Eliminate the global singleton caches in `render_island.ts` that
can cause stale module reuse across sessions or framework changes. This is
a **prerequisite** for Phase 4's shared pipeline which will call these
loaders from inside `runGui`.

**Files**:
- `studio/src/lib/gui/render_island.ts`:
  - Replace `activeProtocol: { module, blobUrls } | null` and
    `activeHostBridge: { module, blobUrls } | null` singletons with
    per-call returns (no global cache).
  - Alternatively: key caches by `(entryPath + protocolPath/hostBridgePath)`
    and add an explicit `invalidateModuleCache()` call on session teardown.
  - Fetch VFS snapshot **once** per call group by accepting a pre-fetched
    `VfsSnapshot` parameter in `loadProtocolModule` and `loadHostBridgeModule`
    instead of independently fetching.
  - `unloadProtocolModule` / `unloadHostBridgeModule` remain as teardown APIs.

**Validation**: Two consecutive GUI runs with different frameworks do not
reuse each other's protocol or host bridge modules.

### Phase 4: Add `compileGui` export + shared post-compile pipeline

**Goal**: Unify the `runGui` flow across both backends. Both produce
`{ bytecode, framework, extSpecs }`, then a single shared function handles
ext preloading, host bridge, protocol module, and `runGuiFromBytecode`.

**Sub-step 4a — `vo-studio-wasm` compile-only export**:
- `studio/wasm/src/lib.rs` — Add `compile_gui(path) -> CompileGuiOutput`
  that runs the existing compile pipeline but returns before `run_gui_from_bytecode`.
  Returns bytecode, framework contract, WASM ext specs (names + bytes from
  VFS cache).
- `studio/src/lib/studio_wasm.ts` — Add `StudioWasm.compileGui(path)`
  TypeScript wrapper returning the same shape as `GuiCompileResult`.

**Sub-step 4b — shared pipeline function**:
- `studio/src/lib/gui/gui_pipeline.ts` (new):
  ```typescript
  async function executeGuiFromCompileOutput(
    wasm: StudioWasm,
    compiled: GuiCompileResult,
    backend: Backend,
    runtime: RuntimeService,
    sessionGuard: () => void,
  ): Promise<GuiRunOutput>
  ```
  Steps inside:
  1. Fetch VFS snapshot once via `backend.getRenderIslandVfsSnapshot()`
  2. `wasm.preloadExtModule()` for each ext spec
  3. `loadHostBridgeModule(snapshot)` + `setActiveHostBridge()` if declared
  4. `loadProtocolModule(snapshot)` + `runtime.setProtocolModule()` if declared
  5. `wasm.runGuiFromBytecode(bytecode)`
  6. Extract `hostWidgetHandlerId` via protocol module from render bytes
  7. Return complete `GuiRunOutput`

- `studio/src/lib/backend/native_backend.ts` — `runGui()` becomes:
  `cmd_compile_gui` → `executeGuiFromCompileOutput`

- `studio/src/lib/backend/web_backend.ts` — `runGui()` becomes:
  `wasm.compileGui()` → `executeGuiFromCompileOutput`

- `studio/src/components/PreviewPanel.svelte` — Remove `loadProtocolModule`
  call from `launchRenderIsland` (now done in shared pipeline). Retain
  `unloadProtocolModule` + `runtime.setProtocolModule(null)` in teardown.
  `startRenderIsland` can receive the already-fetched snapshot to avoid
  re-fetch.

**Validation**: Both backends produce identical `GuiRunOutput`. Host bridge
and protocol module are active before initial render on both platforms.
Only one VFS snapshot fetch per GUI launch.

### Phase 5: Lift WASM ext build to explicit prepare step

**Goal**: `cargo build` / `wasm-pack` side-effects are not hidden inside
what appear to be pure read operations.

**Files**:
- `studio/src-tauri/src/commands/gui.rs`:
  - Extract a private `prepare_wasm_extensions(extensions)` function that
    runs `ensure_standalone_wasm_ext_fresh` and calls `ensure_pkg_island_fresh`
    for all extensions discovered from compile output.
  - `cmd_compile_gui` calls `prepare_wasm_extensions` explicitly after
    compile, before `extract_wasm_ext_specs`. `read_wasm_ext_spec` becomes
    a pure read.
  - Remove `ensure_standalone_wasm_ext_fresh` call from inside
    `read_wasm_ext_spec`.
  - Remove `ensure_pkg_island_fresh` call from
    `cmd_get_render_island_vfs_snapshot`.

**Note**: Extension discovery happens inside `prepare_and_compile` output,
so prepare runs *after* compile but *before* ext byte reading — not strictly
before compile. The doc previously said "before compile" which was incorrect.

**Validation**: `read_wasm_ext_spec` contains no `std::process::Command`
calls. `cmd_get_render_island_vfs_snapshot` contains no build calls.

## Migration Risk Assessment

| Phase | Risk | Mitigation |
|-------|------|------------|
| 1 | New WASM export may not match native compile output exactly | Compare bytecode + framework output in tests |
| 2 | Shared flow may break native-specific session guard timing | Keep `sessionGuard()` callback, test race conditions |
| 3 | Stale VFS cache could serve outdated files | Invalidate on session change, not on timer |
| 4 | Separating build from read may break incremental build detection | Preserve mtime-based freshness checks |
| 5 | Removing commands may break unreleased features using them | Grep for all command names before deletion |
| 6 | Low risk | — |

## Open Questions

1. **Should `compileGui` in `vo-studio-wasm` return WASM ext bytes inline
   or as VFS paths?** Inline bytes match the native `cmd_compile_gui`
   contract but increase WASM memory pressure. VFS paths require a
   subsequent fetch step.

2. **Should the protocol module be loaded in the shared post-compile flow
   or remain in `PreviewPanel`?** Loading it earlier (Phase 2) fixes the
   `hostWidgetHandlerId` issue. But it means the shared flow needs
   access to `Backend` for VFS snapshot, coupling compile-time and
   render-time concerns.

3. **Should `cmd_compile_gui` and `wasm.compileGui` share a TypeScript
   result type?** Currently `GuiCompileResult` is native-only (includes
   `wasmExtensions` with raw bytes). A shared type would enforce structural
   parity.
