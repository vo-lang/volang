# Studio Refactor Plan From First Principles

**Date**: 2026-03-14  
**Status**: Draft — Approved for implementation start  
**Scope**: Full Studio architecture redesign, independent of the `toolchain` migration

---

## 1. Problem Statement

Studio should not be shaped around incidental implementation seams such as `bridge.ts`, the shell protocol, or the current split between Tauri commands and WASM exports.

Studio is a product with a small number of core responsibilities:

1. Open and edit a workspace or target.
2. Understand whether the target is a single file or a project.
3. Run code as either a console app or a GUI app.
4. Show build/runtime diagnostics and textual output.
5. Support terminal-style operations for power users.
6. Support launch/import flows for local and remote targets.
7. Work on both native desktop and browser WASM with the same user-facing semantics.

The current codebase satisfies many of these behaviors, but the architecture is organized around implementation shortcuts rather than product boundaries.

---

## 2. First-Principles Requirements

### 2.1 Product Requirements

Studio must provide the following stable product capabilities:

- Open a target from a workspace, a local path, or a launch URL.
- Resolve whether that target is:
  - a single-file program
  - a project rooted by `vo.mod`
- Provide editing primitives:
  - read file
  - write file
  - rename / delete / create
  - browse directory tree
- Provide execution primitives:
  - prepare dependencies
  - run console apps
  - run GUI apps
  - stop a running GUI app
- Provide preview/runtime primitives:
  - receive GUI render bytes
  - send GUI events
  - support island transport and render-island preview
- Provide terminal/tooling primitives:
  - structured shell protocol for `fs.*`, `vo.*`, `git.*`, `zip.*`, `proc.*`, `http.*`
- Provide environment portability:
  - same user semantics on Tauri and WASM
  - explicit capability differences when strict parity is impossible

### 2.2 Non-Functional Requirements

- UI code must not know whether the backend is Tauri or WASM.
- Terminal concerns must not define the whole Studio architecture.
- Preview/runtime concerns must not leak timer/event details into unrelated frontend code.
- Workspace root, session root, imported launch targets, and dev-mode local targets must have explicit semantics.
- Native and WASM implementations should differ in mechanism, not in architectural shape.
- Failure modes must be explicit and typed.
- Backend capabilities should have a single source of truth.

### 2.3 Non-Goals

These are not goals of this refactor:

- Re-centering Studio architecture around `toolchain`
- Rewriting all execution internals at once
- Changing the shell protocol unless required by clearer boundaries
- Redesigning the UI/UX surface first

---

## 3. Current Architecture Audit

### 3.1 What Exists Today

Today Studio is composed of these major pieces:

- **Frontend UI**
  - `App.svelte`
  - Svelte stores in `stores/ide.ts` and `stores/explorer.ts`
  - actions in `src/lib/actions/*`
- **Bridge / bootstrap**
  - `src/lib/bridge.ts`
- **Terminal shell subsystem**
  - `src/lib/shell/protocol.ts`
  - `src/lib/shell/client.ts`
  - `src/lib/shell/transport.ts`
  - `src/lib/shell/wasm/*`
  - `studio/src-tauri/src/shell/*`
- **Native runtime path**
  - dedicated Tauri commands in `studio/src-tauri/src/lib.rs`
  - native GUI runtime in `studio/src-tauri/src/gui_runtime.rs`
- **WASM runtime path**
  - Studio wasm exports in `studio/wasm/src/lib.rs`
  - ext-module host glue in `src/lib/ext-modules.ts`
- **Preview / render-island path**
  - `src/lib/render_island.ts`

### 3.2 The Main Architectural Problems

#### A. `bridge.ts` is an accidental god object

`bridge.ts` currently combines:

- runtime detection
- backend bootstrap
- shell client creation
- dev-mode local module closure loading
- launch-mode switching
- WASM shell cache bootstrapping
- GUI timer/event platform glue
- WASM host bridge wiring for extension modules
- convenience filesystem facade for the rest of the app

This is too many responsibilities for one module. It is both a composition root and a runtime implementation.

#### B. The shell subsystem is over-central in the app architecture

The shell protocol is a good abstraction for terminal-style commands, but it is currently also used as the default service boundary for core Studio product features such as:

- filesystem actions
- app preparation
- app execution
- GUI execution

This makes the terminal protocol more fundamental than it should be. From a product standpoint, the shell is a subsystem, not the root abstraction of Studio.

#### C. App execution and shell execution are split inconsistently

Today:

- `fs.*`, `vo.*`, `git.*`, `zip.*`, `proc.*`, `http.*` mostly go through shell
- `app.*` and `gui.*` bypass shell and go through dedicated paths
- render-island preview uses a separate Tauri/WASM route again

This means there is no coherent runtime service boundary. There are just multiple ad-hoc escape hatches.

#### D. Workspace root and session root are under-specified

There are at least four concepts in play:

- app workspace root
- current session root
- imported local launch target
- local dev-mode target root

These are real product concepts, but the architecture does not expose them clearly. Instead, they are partially hidden in `bridge.ts`, `launch_import.ts`, and Tauri commands.

#### E. Native and WASM share behavior but not shape

Native and WASM do the same product jobs, but the implementation seams are different enough that drift is easy:

- native uses Tauri commands + Rust guest runtime threads
- WASM uses module exports + JS event/timer glue + shell bytecode cache
- capability declarations are duplicated
- runtime event flow differs materially

The result is parity by convention, not by architecture.

#### F. WASM host/runtime glue is conflated with extension loading

`ext-modules.ts` currently mixes:

- generic external module loading/calling
- host imports for standalone WASM modules
- GUI timer/platform hooks
- compiler/run/VFS bridging globals

These are separate concerns and should not live in one file.

#### G. Frontend state is too flat for the product model

`IdeState` currently mixes:

- workspace/edit state
- console state
- run lifecycle state
- guest render state
- preview transport state

This makes UI actions operate on a single large mutable object instead of a smaller set of product-oriented models.

---

## 4. Design Principles For The New Architecture

1. **Product-first boundaries**
   - workspace, launch, terminal, app runtime, preview runtime are separate subsystems
2. **Terminal is a subsystem**
   - not the base abstraction for all Studio interactions
3. **Composition root is small**
   - bootstrap should wire services, not implement business logic
4. **Backend interface is explicit**
   - Tauri and WASM each implement the same Studio-facing service shape
5. **Runtime responsibilities are isolated**
   - app execution, GUI lifecycle, render-island transport, and ext-module loading should be separate modules
6. **State follows product domains**
   - workspace state, execution state, console state, preview state should be distinct
7. **Migration must be phased**
   - introduce target architecture without destabilizing all features at once

---

## 5. Target Architecture

## 5.1 Top-Level Architecture

```text
UI Components / Actions
        │
        ▼
StudioFrontend Services
        │
        ▼
StudioBackend
├── WorkspaceService
├── LaunchService
├── TerminalService
├── AppRuntimeService
├── PreviewRuntimeService
└── CapabilityService
        │
        ├── TauriStudioBackend
        └── WasmStudioBackend
```

The core architectural shift is:

- **UI/actions talk to `StudioBackend` services**
- **Terminal uses `TerminalService`**
- **App run / GUI run / preview use runtime services**
- **The old `bridge.ts` becomes only a temporary temporary bridge during migration**

## 5.2 Service Boundaries

### A. `WorkspaceService`

Responsibilities:

- list/stat/read/write/mkdir/remove/rename
- workspace target resolution
- single-file vs project detection
- local workspace/session root exposure

This service is for editor/explorer/workspace actions.

### B. `LaunchService`

Responsibilities:

- materialize imported targets
- activate local dev mode
- reset launch mode
- expose launch/session metadata

This service owns the semantics of imported targets and dev-mode session switching.

### C. `TerminalService`

Responsibilities:

- typed command execution for shell ops
- streaming events
- capability gating for terminal operations

The shell protocol remains here. It is not removed. It is just scoped correctly.

### D. `AppRuntimeService`

Responsibilities:

- prepare entry target
- run console app
- run GUI app
- stop GUI app
- send GUI event / async GUI event

This service defines the runtime contract for the main Studio preview/run flow.

### E. `PreviewRuntimeService`

Responsibilities:

- render-island VFS sync
- island transport wiring
- host event replay for preview widget
- preview lifecycle

This should not be owned by UI widgets directly.

### F. `CapabilityService`

Responsibilities:

- provide one canonical capability set
- expose backend-specific support
- avoid duplicated capability lists across protocol/router/native bootstrap

---

## 6. Required Codebase Restructure

## 6.1 Frontend Structure

Create a new frontend backend layer:

```text
studio/src/lib/backend/
  types.ts
  index.ts
  tauri_backend.ts
  wasm_backend.ts
  bootstrap.ts
  services/
    workspace.ts
    launch.ts
    terminal.ts
    runtime.ts
    preview.ts
```

Rules:

- actions and components import `studioBackend()` instead of `bridge()`
- `bridge.ts` becomes an adapter during migration, then is removed or collapsed into bootstrap
- shell-specific files remain under `shell/`
- preview-specific files move under `backend/services/preview*` or `preview/`

## 6.2 Native Structure

Refactor Tauri commands into clear modules:

```text
studio/src-tauri/src/
  backend/
    workspace.rs
    launch.rs
    terminal.rs
    runtime.rs
    preview.rs
```

Rules:

- shell handler runner remains terminal-specific
- GUI guest lifecycle remains runtime-specific
- launch/session-root logic moves out of monolithic `lib.rs`
- capability declaration becomes centralized

## 6.3 WASM Structure

Refactor the browser path into explicit runtime modules:

```text
studio/wasm/src/
  backend/
    workspace.rs
    terminal.rs
    runtime.rs
    preview.rs
```

And split frontend JS host glue:

```text
studio/src/lib/
  wasm/
    ext_bridge.ts
    runtime_host.ts
    gui_platform.ts
    shell_cache.ts
```

Rules:

- ext module loading and runtime host globals are separate
- shell-handler cache is not mixed with backend composition
- GUI timer/event platform is not mixed with ext loading

---

## 7. Migration Plan

## Phase 1 — Introduce The New Frontend Backend Boundary

Goal:

- establish `StudioBackend` and service interfaces
- stop new code from depending on `bridge.ts`
- migrate core actions (`workspace`, `exec`, `launch_import`) to the new boundary

This is the first implementation slice to start immediately.

## Phase 2 — Split Native Backend By Domain

Goal:

- extract workspace/launch/runtime/terminal commands from `src-tauri/src/lib.rs`
- preserve current behavior while making backend domains explicit

## Phase 3 — Split WASM Runtime Glue By Domain

Goal:

- separate shell cache, ext bridge, runtime host bridge, GUI event platform, and preview transport
- remove `bridge.ts` ownership of WASM internals

## Phase 4 — Make Terminal A First-Class Subsystem, Not The App Core

Goal:

- keep `ShellClient` for terminal and scripting use
- stop editor/workspace/runtime services from routing through shell by default

## Phase 5 — Rebuild Preview / Render-Island Ownership

Goal:

- move preview lifecycle out of widget-local ad hoc logic
- define a proper preview runtime service shared by Tauri and WASM

## Phase 6 — State Model Cleanup

Goal:

- split `IdeState` into smaller domain-oriented stores
- remove execution/preview/workspace cross-coupling from one flat state object

---

## 8. Risks And Watchpoints

### Risk 1: Backend parity drift

Mitigation:

- keep a single frontend service interface
- make Tauri and WASM adapters implement the same TypeScript contracts

### Risk 2: Launch/session semantics regress

Mitigation:

- preserve current behavior first
- explicitly test imported local targets, dev mode, and workspace reset

### Risk 3: Preview/render island breaks during service extraction

Mitigation:

- do not rewrite island transport in Phase 1
- first move ownership, then simplify internals

### Risk 4: Terminal regressions while decoupling shell

Mitigation:

- keep `ShellClient` and shell protocol intact
- only change who depends on it

---

## 9. Immediate Implementation Start

The first implementation slice starts with:

1. Add `StudioBackend` interfaces and composition root.
2. Add Tauri/WASM backend adapters that wrap current implementation.
3. Move frontend actions from `bridge()` to `studioBackend()`.
4. Keep behavior unchanged while changing the architectural seam.

This is the correct first move because it creates the stable boundary required for all later native/WASM/runtime cleanup.

---

## 10. Summary

The Studio refactor should not be framed as “adapting to `toolchain`.”

The real problem is that Studio currently has:

- a god bootstrap module
- a shell protocol that is too central
- runtime services split across ad hoc escape hatches
- mixed launch/workspace/session semantics
- mixed WASM host/ext/runtime glue

The new architecture makes Studio product-oriented again:

- `WorkspaceService`
- `LaunchService`
- `TerminalService`
- `AppRuntimeService`
- `PreviewRuntimeService`
- `CapabilityService`

Implementation starts by introducing `StudioBackend` on the frontend and migrating the app to depend on that boundary instead of `bridge.ts`.
