# From `vox` to stdlib `toolchain`, then Studio, then VoPlay — Refactor Plan

**Date**: 2026-03-14
**Status**: Design — Approved direction

---

## 1. Decision Summary

This refactor will be executed in **three strict steps**:

1. **Toolchain (migrated from `vox`)**
2. **Studio**
3. **VoPlay**

The chosen Studio direction is **Option B**:

- Studio provides a **generic external-island host**.
- Studio must not contain `voplay`-specific business logic.
- `voplay` keeps split-island protocol, resource proxying, and render-island semantics.

Temporary breakage policy:

- **Studio is allowed to be broken between steps.**
- **Volang itself must stay clean, internally consistent, and architecturally correct at every step.**

This means Step 1 is allowed to delete or invalidate old Studio integration points if those points are part of the architectural debt.

---

## 2. Why This Refactor Is Necessary

The current layering is not clean enough.

### 2.1 Current `vox` problem

The external `vox` repo currently mixes multiple responsibilities:

- Public Vo package surface (`vox/vox.vo`)
- Compiler/runner facade (`vox/rust/src/lib.rs` re-exporting `vo_engine`)
- AST and bytecode inspection helpers (`vox/rust/src/ffi.rs`)
- Native GUI guest runtime (`vox/rust/src/gui.rs`)

That combination was convenient while bootstrapping, but it is not the right long-term architecture.

The future stdlib package derived from `vox` is supposed to expose **compiler-facing functionality to Vo code**. It should not be the place where long-lived Studio GUI hosting policy lives.

### 2.2 Current Studio problem

Studio currently contains `voplay` knowledge in places where it should be host-generic.

Examples in the current code:

- `studio/src/lib/render_island.ts` registers `voplay-render-island`
- `studio/src-tauri/src/gui_runtime.rs` probes `vo_voplay_set_studio_mode`

That means Studio is not acting as a neutral host. It is acting as a partially hardcoded app-specific runtime.

### 2.3 Current `voplay` problem

`voplay` correctly owns split-island behavior today, but it still relies on Studio-specific host signaling:

- `voplay/host.vo` calls `useStudioExternalWidget()`
- `voplay/rust/src/lib.rs` exports `vo_voplay_set_studio_mode`

That is backwards. `voplay` should depend on **generic host capabilities**, not a Studio-specific mode toggle.

---

## 3. Hard Constraints

These constraints are non-negotiable.

### 3.1 No dependency cycle inside `volang`

Today `vo-engine` depends on `vo-stdlib`:

- `vo-engine/Cargo.toml` depends on `vo-stdlib`
- `vo-engine` uses `vo_stdlib::EmbeddedStdlib`

Therefore, after moving `vox` into stdlib as `toolchain`, we must **not** create a new `vo-stdlib -> vo-engine` dependency.

This is the most important technical constraint for Step 1.

### 3.2 `toolchain` in stdlib means public package surface, not host policy dumping ground

Moving `vox` into stdlib as public package `toolchain` does **not** mean copying the whole external `vo-vox` crate into `vo-stdlib` unchanged.

The public `toolchain` package must be narrowed to its real responsibility:

- compiler access from Vo
- module inspection
- parsing / formatting / bytecode tooling
- project bootstrap / module install helpers where appropriate

It must **not** become the home of Studio guest GUI runtime policy.

### 3.3 Studio must become host-generic

Studio may own:

- external widget host plumbing
- secondary VM boot
- island transport
- host event pumping
- VFS and module hooks
- generic capability publication

Studio must not own:

- `voplay` widget names
- `voplay` render protocol
- `voplay` resource protocol
- `voplay` mode toggles

### 3.4 `voplay` must keep render-island business semantics

`voplay` must continue to own:

- split-island lifecycle
- render worker protocol
- resource proxy protocol
- render-island business semantics
- asset loading semantics above the generic host VFS layer

### 3.5 Existing package identity / ABI rules stay intact except for the intentional `vox` -> `toolchain` public rename

This refactor must not redefine package identity or extern ABI naming rules.

In particular:

- the new stdlib public package for this functionality becomes `toolchain`
- canonical package path remains the authoritative language/module identity
- `abi_path` remains the ABI namespace identity
- extern lookup naming remains driven by the existing ABI-path logic

The rename from external `vox` to stdlib `toolchain` is an intentional package-surface change. Beyond that rename, this refactor is about ownership and layering, not a general change to package naming semantics.

---

## 4. Current Architecture Snapshot

### 4.1 `vox` today

Current facts:

- `vox/vox.vo` exposes compile/run/AST/bytecode/project APIs and also GUI APIs
- `vox/rust/src/lib.rs` re-exports `vo_engine::{compile, run, ...}`
- `vox/rust/src/ffi.rs` exposes compiler/runtime functions to Vo as `package vox`
- `vox/rust/src/gui.rs` owns native GUI guest-thread runtime and extension-host configuration hooks

This means the current `vox` surface is wider than “compiler interface to Vo”, and that wide surface should not be carried forward unchanged into stdlib `toolchain`.

### 4.2 Stdlib

Current facts:

- stdlib Vo sources live in `lang/stdlib/`
- stdlib package index lives in `lang/stdlib/stdlib.toml`
- runtime/native side lives in `lang/crates/vo-stdlib`

There is currently **no stdlib `toolchain` package** under `lang/stdlib/`.

### 4.3 Studio

Current facts:

- `studio/src-tauri` currently depends on `vo-vox`
- `studio/wasm/src/lib.rs` already exports generic host-side compiler entry points such as `voHostCompileFile`, `voHostCompileString`, `voHostRunBytecode`, etc.
- `studio/src/lib/render_island.ts` still registers a `voplay`-specific external widget
- `studio/src-tauri/src/gui_runtime.rs` still reaches into `voplay` via `vo_voplay_set_studio_mode`

### 4.4 `voplay`

Current facts:

- `voplay/host.vo` owns split-host behavior and widget event handling
- `voplay/game.vo` owns split-island setup and resource proxy wiring
- `voplay/rust/src/externs/render.rs` exposes `useStudioExternalWidget`
- `voplay/rust/src/lib.rs` exports `vo_voplay_set_studio_mode`

This confirms that `voplay` already owns the business semantics, but host capability discovery is still wrong.

---

## 5. Target Architecture

### 5.1 Final ownership model

```text
Vo code
  |
  +-- stdlib package: toolchain
  |     Public compiler / parser / formatter / bytecode API for Vo code
  |
  +-- Studio
  |     Generic GUI host + generic external-island host
  |
  +-- voplay
        Game-engine business logic, split-island protocol, render-island semantics
```

### 5.2 `toolchain` target role

`toolchain` becomes a **stdlib package and stdlib-backed host ABI surface**, sourced from the cleanup of today's external `vox`.

Its role is:

- expose compiler services to Vo code
- expose parse / print / format services to Vo code
- expose bytecode load/save/format helpers to Vo code
- expose module/project bootstrap helpers to Vo code

Its role is **not**:

- hosting GUI guest sessions for Studio
- owning render-island policy
- owning app-specific runtime bridges

### 5.3 Studio target role

Studio becomes a **generic host runtime**.

It owns:

- GUI session hosting
- external widget hosting
- generic external-island VM boot
- island transport
- VFS setup and workspace mirroring
- module-driven wasm extension loading
- generic host capability publication

Studio must not know that `voplay` exists.

### 5.4 `voplay` target role

`voplay` remains the owner of:

- split-island boot policy
- render-island lifecycle
- resource proxying and request/response formats
- surface/input/frame semantics
- any game-engine-specific render behavior

`voplay` must stop depending on a Studio-specific flag and start depending on a generic host capability.

---

## 6. Step 1 — Move `vox` into stdlib as `toolchain` and clean its responsibility

This is the highest-priority step.

### 6.1 Public result of Step 1

After Step 1:

- `toolchain` exists under `lang/stdlib/toolchain`
- `lang/stdlib/stdlib.toml` registers `toolchain`
- `vo-stdlib` owns the `package toolchain` ABI bridge
- `volang` no longer relies on the external repo as the architectural home of this public compiler-tooling package
- `toolchain` is narrowed to compiler-facing tooling
- `vox` remains only as migration/history context unless an explicit temporary compatibility alias is required
- Studio may be temporarily broken

### 6.2 Core design: host-installed `toolchain` backend

Because `vo-engine` already depends on `vo-stdlib`, Step 1 must avoid a cycle.

The correct pattern is:

- `vo-stdlib` defines a **host trait** for `toolchain` operations
- embedders install an implementation of that trait
- the stdlib `toolchain` externs forward into that installed host

Conceptually:

```text
lang/stdlib/toolchain/toolchain.vo
        |
        v
vo-stdlib (ABI bridge + handle storage + trait definitions)
        |
        v
installed Toolchain host implementation
        |
        v
vo-engine / vo-syntax / vo-vm / project tooling
```

### 6.3 What should live in `vo-stdlib`

`vo-stdlib` should own:

- the public `toolchain` package source embedding
- extern registration for `package toolchain`
- opaque handle storage for compiled modules / parsed ASTs / temporary objects
- the `ToolchainHost`-style trait definition
- installation hooks such as `install_toolchain_host(...)`
- ABI marshalling between Vo values and host calls

### 6.4 What should *not* live in `vo-stdlib`

`vo-stdlib` should not directly own:

- a concrete dependency on `vo-engine`
- Studio-native GUI session hosting
- Tauri-specific behavior
- render-island boot logic

### 6.5 Proposed `toolchain` public API boundary

The stdlib `toolchain` package should cover:

- compile file / dir / string
- compile check
- run / run capture
- parse file / string
- print AST
- format source
- bytecode load / save / inspect
- project init / module get

The stdlib `toolchain` package should **not** carry the current GUI runtime API as part of its long-term responsibility.

That means these APIs do not belong in stdlib `toolchain` as public compiler-facing surface:

- `RunGui`
- `SendGuiEvent`
- `StopGui`

If temporary compatibility shims are needed during migration, they must be clearly marked as host-private transitional APIs, not core stdlib design. If a temporary `package vox` alias exists at all, it must be treated as deprecated migration glue rather than the final public surface.

### 6.6 Step 1 deliverables

1. Add `lang/stdlib/toolchain/` with the public Vo package.
2. Register `toolchain` in `lang/stdlib/stdlib.toml`.
3. Implement `package toolchain` extern bridge in `vo-stdlib`.
4. Introduce installed-host abstraction for `toolchain` operations.
5. Move compile/parse/format/bytecode/project APIs behind that abstraction.
6. Remove GUI session hosting from the architectural definition of `toolchain`.
7. Update internal users in `volang` to stop treating the external `vo-vox` repo as the source of truth for the public package surface.
8. If a temporary `vox` compatibility layer is kept, mark it deprecated and remove it once downstream callers are migrated.

### 6.7 Step 1 acceptance criteria

- `volang` has an internal stdlib `toolchain` package
- no `vo-stdlib -> vo-engine` cycle exists
- compile/run/check from Vo still work through `package toolchain`
- AST and bytecode helpers still work
- GUI hosting is no longer considered part of `toolchain`'s core responsibility
- if `package vox` still exists, it is clearly transitional and not the source of truth
- Studio breakage is acceptable at this stage

---

## 7. Step 2 — Clean Studio into a generic host (Option B)

This step starts only after Step 1 has established the clean `toolchain` boundary.

### 7.1 Public result of Step 2

After Step 2:

- Studio hosts external islands generically
- Studio no longer names `voplay`
- Studio no longer pokes `voplay`-specific native symbols
- Studio prepares VFS/module hooks generically before starting the secondary wasm VM

### 7.2 Required removals

These patterns must disappear from Studio:

- `registerWidget('voplay-render-island', ...)`
- `vo_voplay_set_studio_mode`
- any direct `voplay` symbol probing
- any special-case render-island bootstrap that assumes `voplay`

### 7.3 What Studio should provide instead

Studio should provide a generic host layer with:

- generic external widget hosting
- generic external-island VM startup
- generic island transport
- generic timer / host-event scheduling
- generic VFS setup
- generic local module / local wasm hooks
- generic capability publication to guests

### 7.4 Generic capability contract

Studio must publish capabilities generically, for example along the lines of:

- external widget host available
- external island host available
- secondary wasm guest available
- host VFS available

The contract may live in Studio, `vogui`, or another shared host layer, but it must not be `voplay`-specific.

### 7.5 Step 2 acceptance criteria

- Studio contains no `voplay`-specific host logic
- render-island startup path is module-driven and host-generic
- all VFS/module-preload logic is generic host code
- Studio can host a render island without knowing the app-specific protocol semantics

---

## 8. Step 3 — Clean `voplay` to depend only on generic host capability

This is the final step.

### 8.1 Public result of Step 3

After Step 3:

- `voplay` owns all split-island business semantics
- `voplay` depends only on generic host capability
- `voplay` does not know about Studio-specific mode toggles

### 8.2 Required removals

These patterns must disappear from `voplay`:

- `useStudioExternalWidget()` as a Studio-specific capability test
- `vo_voplay_set_studio_mode`
- any host discovery based on “am I running inside Studio?”

### 8.3 What `voplay` keeps

`voplay` continues to own:

- split-island lifecycle
- logic/render worker contract
- resource proxy protocol
- mount/resize semantics for its render workflow
- asset-loading behavior above host file access

### 8.4 What `voplay` changes

`voplay` changes only its host dependency boundary:

- from: Studio-specific mode
- to: generic external-host capability

Under Option B, Studio can provide a generic external widget host, while `voplay` continues to define the protocol payloads and business behavior.

### 8.5 Step 3 acceptance criteria

- `voplay` contains no Studio-specific host toggle API
- `voplay` can run wherever the generic host capability exists
- all split-island business logic remains inside `voplay`
- Studio remains a neutral platform

---

## 9. Migration Rules

These rules apply throughout all three steps.

### 9.1 Cleanliness beats temporary compatibility

If a temporary compatibility layer keeps the old design alive in the wrong place, delete it.

Temporary Studio breakage is acceptable.

### 9.2 Do not move business logic into Studio

When something is needed to make `voplay` work, first ask whether it is:

- a generic host capability
- or `voplay` business logic

Only the first category belongs in Studio.

### 9.3 Do not re-export architectural confusion

The refactor must not replace one mixed-responsibility API with another mixed-responsibility API.

Examples:

- `toolchain` must not remain “compiler API plus GUI runtime plus Studio hooks”
- Studio must not become “generic host plus voplay special cases”

### 9.4 Keep package and ABI semantics stable

Do not change:

- canonical package path rules
- `abi_path` rules
- extern naming rules

Those are separate concerns.

---

## 10. Recommended Implementation Order Inside Step 1

Step 1 itself should be executed in this sub-order:

1. Define the target `toolchain` API boundary.
2. Add stdlib package source under `lang/stdlib/toolchain`.
3. Add `toolchain` entry to `stdlib.toml`.
4. Implement host-installed `toolchain` backend abstraction in `vo-stdlib`.
5. Move compile/check/run/parse/format/bytecode/project functions behind that abstraction.
6. Remove GUI session hosting from the public definition of `toolchain`.
7. Update internal users in `volang` to the new source of truth.
8. Only then begin Studio cleanup.

This order ensures `volang` becomes clean before Studio is repaired.

---

## 11. Short Version

- public `toolchain` becomes the **stdlib compiler-tooling package**, not a GUI host.
- `vo-stdlib` owns the `toolchain` ABI surface through an **installed host backend**, not through a direct `vo-engine` dependency.
- Studio becomes a **generic external-island host**.
- `voplay` keeps **all split-island and render-island business logic**.
- Studio may break in the middle.
- `volang` must stay clean the whole time.
