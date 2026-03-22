# vo-app-runtime Implementation Plan
 
 > Status: Phase 1 and Phase 2 Implemented; Phase 3 Split Deferred  
 > Date: 2026-03-21  
 > Related: `2026-03-14-studio-rewrite-design.md`, `2026-03-13-island-channel-no-std-design.md`

---

## 1. Goal

Introduce a shared `vo-app-runtime` layer that owns GUI app session/runtime orchestration across native Studio and Studio WASM, without absorbing module-system concerns, shell/frontend concerns, or host-specific transport details.

This work is not a cosmetic rename of `vo-web` or `gui_runtime.rs`. It is a structural extraction of the shared runtime kernel currently duplicated across:

- `studio/src-tauri/src/gui_runtime.rs`
- `studio/wasm/src/lib.rs`

 The first implementation phase will establish the shared core abstraction, move the outcome/effect model into the new crate, and wire Studio native/WASM to consume that core incrementally.

### 1.1 Progress Update (2026-03-22)

Completed since this plan was opened:

- `vo-app-runtime` now owns the shared session/runtime kernel, explicit effect model, dispatch helpers, and guest/render-island session abstractions
- Studio WASM now consumes the shared web adapter layer in `vo-app-runtime::web`
- Studio native now consumes the shared native adapter layer in `vo-app-runtime::native`
- wasm GUI runtime wiring now reaches the web backend/loader path, including `runGuiEntry`, `sendGuiEvent`, `pushIslandTransport`, and `pollGuiRender`
- inbound island/event generated render bytes are now surfaced to the frontend in wasm mode instead of being dropped at the shell boundary
- web mode now receives canonical wasm-side GUI metadata (`moduleBytes`, `framework`, render-island VFS snapshot) instead of reconstructing partial metadata in TypeScript

Remaining work after this update is no longer about extracting more shared runtime logic from Studio shells. The main open decision is whether the adapter modules should become separate crates.

---

## 2. Current Boundary Problems

### 2.1 Native Studio runtime mixes three layers

`studio/src-tauri/src/gui_runtime.rs` currently mixes:

- shared guest session semantics
- native adapter logic
- Studio/Tauri shell glue

Concrete examples:

- `SchedulingOutcome` handling is generic runtime logic
- `TickLoopControl` and extension bridge installation are native adapter logic
- `AppHandle` / `Emitter` interactions are shell glue

### 2.2 Studio WASM runtime duplicates the same session kernel

`studio/wasm/src/lib.rs` currently owns:

- guest VM lifecycle
- GUI event dispatch
- host-event wakeup handling
- island transport drain/push
- stdout draining
- render-island runtime state

This is the same kernel domain as native Studio, but implemented independently.

### 2.3 Existing abstractions are already signaling the right cut

The current codebase already provides the ingredients needed for the extraction:

- `vo-vm` already exposes host-event wake, pending-host-event drain, island command drain, island id state, and scheduled execution
- `studio/src/lib/backend/backend.ts` already defines a runtime-facing API surface shared by native and web backends
- `vo-module` and `vo-engine` already represent separate concerns from app-runtime orchestration

The missing piece is the shared runtime/session layer.

---

## 3. Non-Goals

The new `vo-app-runtime` layer must not absorb the following responsibilities:

### 3.1 Module system and dependency lifecycle

Keep in:

- `vo-module`
- `vo-engine`
- `vo-web`

Examples:

- registry access
- lockfile validation
- frozen-build validation
- source package materialization
- project dependency preparation

### 3.2 Host shell and frontend renderer responsibilities

Keep in Studio/native/TS layers:

- Tauri command plumbing
- `AppState` ownership
- browser `Blob` URL materialization
- JS renderer loading
- widget registration
- DOM/canvas host surfaces

### 3.3 Native extension build policy

Keep in native toolchain/runtime layers:

- extension compilation
- native artifact validation
- `ExtensionLoader` construction policy

---

## 4. Target Layering

### 4.1 Layer overview

```text
Studio shell / product glue
  ├── Tauri commands / AppState / backend.ts implementations
  ├── browser renderer host / widget host / preview plumbing
  └── session ownership and transport integration

vo-app-runtime adapters
  ├── native adapter
  └── web adapter

vo-app-runtime core
  ├── GUI session state machine
  ├── render-island session state machine
  ├── outcome interpretation
  ├── pending host-event extraction
  ├── outbound island frame extraction
  └── shared runtime effect model

lower-level primitives
  ├── vo-vm
  ├── vo-runtime
  ├── vo-web
  ├── vo-engine
  └── vo-module
```

### 4.2 Crate strategy

Start with a single new crate:

- `lang/crates/vo-app-runtime`

Internal modules in phase 1:

- `core`
- `native` (placeholder only if immediately useful)
- `web` (placeholder only if immediately useful)

We intentionally avoid starting with three separate crates. The abstraction boundary should stabilize before physical crate splitting.

---

## 5. Phase 1 Scope

Phase 1 creates a shared runtime kernel and integrates the first common pieces without forcing a full migration.

### 5.1 Deliverables

- new `vo-app-runtime` crate in `lang/crates`
- shared session effect/data types
- shared `SchedulingOutcome` interpretation helper
- shared pending host-event extraction helper
- shared outbound island-frame extraction helper
- shared stdout/render-output collection policy for app sessions
- first consumers in Studio native and Studio WASM

### 5.2 Explicitly deferred

- console runtime unification
- Tauri command refactor beyond what is needed to compile
- browser renderer host extraction
- full native tick-loop migration
- full render-island API redesign
- full wasm-bindgen API redesign

---

## 6. Core API Shape

### 6.1 Session effects

The core should expose runtime results as explicit effects rather than performing host actions directly.

Candidate shape:

```rust
pub struct SessionAdvance {
    pub replay_event_wait_token: Option<u64>,
    pub pending_host_events: Vec<PendingHostEvent>,
    pub outbound_island_frames: Vec<Vec<u8>>,
    pub stdout: Option<String>,
}

pub struct PendingHostEvent {
    pub token: u64,
    pub delay_ms: i64,
}
```

The exact type names may change during implementation, but the direction is fixed:

- extract effects from VM state
- leave transport/execution of those effects to adapters/shells

### 6.2 VM-facing abstraction

Phase 1 should keep the VM-facing abstraction thin.

Preferred direction:

- small helper functions generic over the actual `Vm` type
- avoid a large `Platform` trait
- avoid introducing a large dyn-dispatched trait before it is proven necessary

### 6.3 Outcome semantics

Shared runtime rules:

- `Completed`, `Suspended`, `SuspendedForHostEvents` are acceptable session outcomes
- `Blocked` is a runtime error and must surface `deadlock_err()`
- `Panicked` is a hard failure for bounded app-session execution

This logic currently exists in Studio WASM and should become the canonical shared definition.

---

## 7. Migration Plan

### Phase 1A — Introduce shared core crate

- create `vo-app-runtime`
- add phase-1 types and helpers
- keep adapters in Studio for now

### Phase 1B — Reuse shared outcome/effect extraction in Studio WASM

Move these behaviors to `vo-app-runtime`:

- `handle_guest_outcome` semantic core
- pending host-event extraction
- replay token detection
- outbound island frame collection
- stdout collection normalization

Keep in Studio WASM for now:

- `wasm_bindgen` exports
- VFS compile entrypoints
- JS object encoding for pending host events
- global guest singleton ownership

### Phase 1C — Reuse shared outcome/effect extraction in native Studio

Move these behaviors toward the shared crate:

- post-run/session-step outcome interpretation
- shared render-output extraction policy
- shared island transport frame handling policy where possible

Keep in native Studio for now:

- thread/channel ownership
- `TickLoopControl`
- `TickProvider`
- Tauri `AppHandle` interaction
- extension bridge install/cleanup

### Phase 2 — Extract adapter layers

Status: Implemented.
 
 After phase 1 is stable:
 
- move native runtime adapter code under `vo-app-runtime`
- move WASM/runtime adapter code under `vo-app-runtime`
- shrink Studio-side runtime files into shell/product glue

Completed result:

- `vo-app-runtime::native` owns the shared native guest runtime adapter logic
- `vo-app-runtime::web` owns the shared wasm guest/render-island adapter logic
- Studio-side runtime files are now primarily shell/product glue (`wasm_bindgen` exports, JS encoding, singleton ownership, Tauri glue, thread/channel ownership)

### Phase 3 — Decide crate split

Status: Decision recorded.
 
 If the separation remains stable and dependency direction is clean:
 
- optionally split `vo-app-runtime-native`
- optionally split `vo-app-runtime-web`

Not before.

Current decision:

- do not split `vo-app-runtime` into native/web crates yet

Rationale:

- the adapter modules are stable, but still thin enough that a crate split would mostly add packaging overhead
- dependency direction is already clean inside a single crate
- the remaining platform-specific logic still lives above the adapters in Studio/native/TS shell layers, not inside Rust adapter modules
- revisit the split only if native/web adapters start to accumulate materially different dependencies, test matrices, or release cadence

---

## 8. File-Level Migration Targets

### 8.1 From `studio/wasm/src/lib.rs`

Extract first:

- outcome interpretation logic
- event wait token tracking logic
- pending host-event queue population logic
- outbound island frame drain logic

Keep in place initially:

- `run_gui_entry`
- `send_gui_event`
- `push_island_data`
- `poll_pending_host_event`
- `wake_host_event`
- `stop_gui`

### 8.2 From `studio/src-tauri/src/gui_runtime.rs`

Extract later in phase 1 or early phase 2:

- shared event dispatch step helpers
- shared render/output draining rules
- common guest-step error normalization

Keep in place initially:

- `GuestHandle`
- `PushReceiver`
- `TickLoopControl`
- `StudioTickProvider`
- native bridge installation
- Tauri-specific emission behavior

---

## 9. Validation Plan

Validation should be done incrementally after each phase.

### 9.1 Compile checks

- `cargo check -p vo-app-runtime --release`
- `cargo check -p vo-web --release`
- `cargo check --release --target wasm32-unknown-unknown` in `studio/wasm`
- native Studio compile validation after the native side starts depending on the new crate

### 9.2 Behavioral checks

- GUI app initial render still arrives on first run
- GUI event dispatch still resumes the blocked guest fiber correctly
- non-replay host events are still surfaced exactly once
- outbound island frames are still emitted in order
- async/game-loop events still preserve current native behavior

### 9.3 Failure policy

Do not add fallback-heavy or defensive behavior. If session state is invalid, fail fast with an explicit error.

### 9.4 Latest validation snapshot (2026-03-22)

Validated after the latest wasm GUI runtime wiring and async render surfacing work:

- `cargo test -p vo-app-runtime --release`
- `cargo test -p vo-app-runtime --release --features std`
- `cargo check --release --target wasm32-unknown-unknown` in `studio/wasm`
- `cargo check --release` in `studio/src-tauri`
- `npm run build` in `studio`

---

## 10. Risks and Controls

### 10.1 Over-abstracting too early

Risk:

- introducing a large trait hierarchy before the real shared API stabilizes

Control:

- phase 1 uses thin helpers and explicit data types

### 10.2 Mixing module lifecycle back into runtime core

Risk:

- `vo-app-runtime` becoming a second toolchain crate

Control:

- project preparation and dependency resolution stay in existing crates

### 10.3 Pulling frontend/browser host logic into Rust

Risk:

- renderer host, widget registration, or Blob URL logic migrates into Rust for convenience

Control:

- browser host remains a TS concern until independently justified

### 10.4 Native/web semantics drifting during migration

Risk:

- moving only one side first creates temporary behavior divergence

Control:

- canonicalize outcome/effect semantics in `vo-app-runtime` first, then make both sides consume the same helpers

---

## 11. Immediate Implementation Order

1. Create `vo-app-runtime` crate.
2. Add phase-1 core types and helpers.
3. Switch Studio WASM `handle_guest_outcome` to shared logic.
4. Introduce native-side shared helpers where they match existing behavior exactly.
5. Re-run compile checks and record remaining extraction points.

---

## 12. Expected End State of Phase 1

At the end of phase 1:

- `vo-app-runtime` exists and owns the canonical GUI session effect model
- Studio WASM no longer defines the authoritative outcome/effect semantics locally
- native Studio begins consuming the same shared rules for the overlapping pieces
- the next migration steps are constrained and mechanical rather than architectural

This end state has been reached, and phase 2 adapter extraction has also been completed. The remaining architecture question is now explicitly narrowed to future crate-splitting policy rather than missing shared-runtime extraction.
