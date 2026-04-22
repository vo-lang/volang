# vo-module / vo-ext / vo-engine / vo-web Architecture Cleanup Plan

**Date**: 2026-03-31
**Status**: In Progress
**Scope**: `vo-module`, `vo-ext`, `vo-engine`, `vo-web`, and the shared extension / module-system boundary between them
**Triggered by**: Cross-crate architecture review focused on boundary drift, duplicated logic, and outdated or thin public surfaces
**Related documents**:
- `lang/docs/dev-notes/2026-03-18-module-system-enforcement-implementation-plan.md`
- `lang/docs/dev-notes/2026-03-30-studio-module-system-review-and-refactor-plan.md`
- `lang/docs/dev-notes/2026-03-31-module-cache-validation-redesign.md`

---

## 1. Why This Document Exists

The current tree is no longer suffering from one giant missing subsystem.
The more important problem is that several major pieces have already landed, but the architectural boundaries between them have not been fully cleaned up.

The review of `vo-module`, `vo-ext`, `vo-engine`, and `vo-web` found a repeated pattern:

- one crate owns the real rule or contract
- another crate re-exposes it from a lower layer
- a third crate cannot reuse the original abstraction directly and reimplements the same logic in a platform-specific way
- thin wrappers remain public after the real shared abstraction already exists

This document defines the cleanup pass needed to turn the current implementation into a coherent architecture instead of a set of landed slices that still carry transitional structure.

This is **not** a minimal bug-fix pass.
It is a boundary-correction and de-duplication pass.

---

## 2. Current Intended Responsibilities

The current crates already point toward a sensible architecture.
The cleanup pass should sharpen that architecture rather than replace it.

### 2.1 `vo-module`

Should be the authoritative home for:

- module identity and version semantics
- `vo.mod` / `vo.lock` / `vo.work` schema and validation
- project dependency discovery
- lock-graph and cache validation rules
- registry protocol rules that are independent of transport
- cache layout and install semantics

### 2.2 `vo-ext`

Should be the authoritative home for:

- extension author SDK
- extension ABI-facing public contract
- host capability bridge contract used by extensions

### 2.3 `vo-engine`

Should be the authoritative home for:

- native compile / check / run pipeline on real filesystems
- native extension preparation for native execution
- compile cache and engine-side diagnostics
- toolchain host integration for stdlib-facing tooling

### 2.4 `vo-web`

Should be the authoritative home for:

- browser / wasm execution and compile integration
- JS VFS integration
- async browser fetch transport
- wasm extension loading in browser environments
- web-specific host logging and wasm bindings

---

## 3. Verified Architectural Problems

The review found the following structural issues.
These are ordered roughly by architectural severity, not by implementation effort.

### P1. `vo-runtime(std)` depends on `vo-module` for extension-manifest semantics

At review time, `vo-runtime::ext_loader` re-exported `vo_module::ext_manifest::{discover_extensions, ExtensionManifest}` and stored that type in its runtime-facing loader state.
That creates a downward dependency problem:

- runtime-level extension loading depends on module-system parsing details
- `vo.ext.toml` contract ownership is blurred between extension SDK and module system
- `vo-engine` ends up obtaining extension manifest types through `vo-runtime`, even though discovery actually lives in `vo-module`

This is the strongest boundary violation found in the review.
Phase 1 has now removed that dependency by introducing a runtime-owned `NativeExtensionSpec` and keeping full manifest ownership in `vo-module`.

### P2. `vo-web` reimplements release / manifest / lock semantics that should be shared rules

`vo-web/src/module_install/fetch.rs` contains platform-specific fetch logic, which is correct.
However, it also contains several pieces of logic that are not browser-specific:
- release-tag construction
- GitHub release asset URL construction
- requested-manifest validation
- locked-manifest validation
- size / digest verification
- reconstruction of `LockedModule` from installed release metadata

These rules overlap with `vo-module` registry, manifest, and lock semantics.
The duplication exists because the current shared abstraction is shaped around synchronous native registry access instead of separating pure protocol rules from transport.

### P3. Native extension preparation is still semantically duplicated inside `vo-engine`

`CompileOutput.extensions` is already produced through frozen-build extension preparation.
But `vo-engine::run` still calls `prepare_extension_manifests(...)` again before constructing an `ExtensionLoader`.

This means the system does not yet have one explicit answer to the question:

- are compile outputs carrying raw extension manifests discovered by analysis?
- or are they carrying already-prepared manifests that are safe to load?

As long as this remains ambiguous, similar logic will continue to be repeated at compile-time and run-time.

### P4. `vo-web` compile pipeline is implemented as four near-duplicate flows

The following public entry points share the same high-level structure:

- `compile_source_with_mod_fs`
- `compile_entry_with_mod_fs`
- `compile_entry_with_vfs`
- `compile_source_with_vfs`

All four perform the same sequence:

- build a `FileSet`
- read project dependencies or project context
- choose a resolver shape
- analyze the project
- codegen the module

The differences are real, but the current implementation encodes them through copy-structured entry points rather than one parameterized internal pipeline.
This makes future semantic fixes easy to miss in one path.

### P5. `vo-module::compat` is becoming a secondary API surface

`vo-module::compat` is not dead code.
It is used by real consumers such as `vo-analysis` and `vo-web`.
But it is structurally a thin wrapper over typed APIs in `identity` and related modules.

The risk is not immediate correctness.
The risk is API drift:

- new callers keep using stringly-typed wrappers
- typed APIs stop being the obvious authoritative surface
- the crate ends up documenting both the real model and a convenience shadow model

### P6. `vo-web` publicly re-exports thin wrappers around `vo_module::project`

`vo-web::WorkspaceProjectContext` and `vo-web::load_workspace_project_context` currently add almost no semantic value beyond renaming `vo_module::project::ProjectContext` and `load_project_context`.

This is a small issue, but it is a classic sign of boundary noise:

- external consumers can learn the wrong crate as the owner of the concept
- later refactors have to maintain two public names for one shared contract

### P7. `vo-engine::toolchain::install_module` preserves a removed install surface outside the authoritative lifecycle

`install_module` still performs a special-case flow:

- parse `module@version`
- fetch manifest directly
- synthesize a one-module `LockFile`
- populate the cache
- discover and prepare extensions locally

The path is still useful for toolchain-host integration, but it bypasses the authoritative module-lifecycle surface owned by `vo-module`.

The `created_by: "vo get"` marker in the synthetic lock construction is also a sign that this path still carries historical command-surface semantics that should no longer be allowed to shape the architecture.

### P8. Small duplicated contracts remain around the edges

The review also found smaller but still real contract duplication:

- extension ABI version is duplicated between `vo-ext` and `vo-runtime`
- native compile logging and web host logging have overlapping but different record schemas
- extension discovery types are imported through different crate paths depending on caller context

These are not the main blockers.
They should be cleaned up only after the larger boundary corrections above land.

---

## 4. Root Cause Summary

The verified problems above mostly come from four deeper causes.

### Root Cause A. The extension contract is split across the wrong layers

`vo.ext.toml` parsing and module-facing extension manifest types belong with `vo-module`, while runtime loading should only depend on a loader-ready native extension spec and a shared ABI constant.
The core mistake was not that the tree lacked one more crate.
The core mistake was that `vo-runtime` imported module-system manifest semantics directly.

That forces at least one of the following bad outcomes:

- `vo-runtime` depends on a higher-level crate
- callers import the same concept from different crate paths
- ABI and manifest rules drift apart over time

### Root Cause B. Shared module-system abstractions stop too high in the stack

The current shared registry abstraction is shaped around native synchronous fetch behavior.
That prevents the browser path from reusing the real rule layer cleanly.

As a result, `vo-web` correctly owns browser transport details but also ends up owning duplicated release / manifest / lock rules that should stay in `vo-module`.

### Root Cause C. Some cross-phase contracts are still implicit instead of typed

The engine compile pipeline and run pipeline both manipulate extension manifests, but the system does not explicitly encode whether those manifests are:

- discovered
- validated
- prepared
- loadable

When the state transition is implicit, duplicate preparation logic is almost guaranteed.

### Root Cause D. Transitional convenience APIs were left public after the real shared contract landed

This affects at least three surfaces:

- `vo-module::compat`
- `vo-web` wrappers over `vo_module::project`
- `vo-engine::toolchain::install_module`

These surfaces are not all wrong by themselves.
The problem is that they prolong transitional architecture and invite new code to depend on the wrong layer.

---

## 5. Target Architecture After Cleanup

The cleanup pass should converge the tree onto the following model.

### 5.1 `vo-module` owns extension manifests; `vo-runtime` owns loader-ready native specs

`vo-module` should remain the owner of:

- `ExtensionManifest`
- `WasmExtensionManifest`
- `WasmExtensionKind`
- `discover_extensions(...)`
- `vo.ext.toml` parsing helpers

`vo-runtime` should own only the runtime-facing loader contract:

- `NativeExtensionSpec`
- `ExtensionLoader`
- the shared ABI version constant sourced from `vo_runtime::ffi`

Expected dependency direction:

- `vo-runtime` does not depend on `vo-module`
- `vo-ext` sources its ABI version from `vo_runtime::ffi`
- `vo-engine`, `vo-web`, `vo-analysis`, and Studio import `ExtensionManifest` from `vo-module`
- `vo-engine` bridges from manifest types to `NativeExtensionSpec` when constructing an `ExtensionLoader`

Critically, `vo-runtime` must no longer depend on `vo-module` just to load native extensions.

### 5.2 `vo-module` owns pure registry / release / lock protocol rules

`vo-module` should own the transport-independent parts of module acquisition:

- release tag and release asset naming rules
- manifest identity validation
- locked-manifest validation
- digest / size verification helpers
- manifest-to-locked-module projection helpers

Native and web should differ only in transport and storage backends, not in rule ownership.

### 5.3 `vo-engine` compile output must carry explicitly prepared extension state

The compile pipeline should output a type whose semantics are unambiguous.
This document will refer to that type as `PreparedExtensionManifest`.

That type should mean:

- the manifest has already passed frozen-build preparation rules
- local build steps that are allowed have already happened
- the native library path is ready for runtime loading

At that point, `run.rs` should only load prepared extensions, not prepare them again.

### 5.4 `vo-web` should keep public entry-point variants, but use one internal compile skeleton

The public API can still expose multiple convenience entry points.
But internally they should flow through one parameterized compile pipeline.

That shared skeleton should abstract over:

- single-file vs package entry
- local module FS vs browser VFS module source
- whether workspace replace roots are available
- stdlib source choice

### 5.5 Transitional wrappers should stop being authoritative surfaces

After the cleanup pass:

- internal users should prefer typed identity / project APIs directly
- `vo-module::compat` should become a narrow interop shim instead of a growing public surface
- `vo-web` should stop publicly fronting `ProjectContext` as if it owns that concept
- engine-side exact-module install helpers should route through authoritative module lifecycle code, not preserve removed `vo get` semantics in place

---

## 6. Phased Execution Plan

## Phase 1 — Remove the `vo-runtime <- vo-module` Edge Without Adding a New Crate

### Goal

Remove the `vo-runtime <- vo-module` dependency edge while making manifest ownership and loader input explicit.

### Required changes

- remove `vo-module` from `vo-runtime` dependencies
- introduce a runtime-owned `NativeExtensionSpec` for loader input and loader state
- keep `ExtensionManifest`, `WasmExtensionManifest`, `WasmExtensionKind`, and discovery / parse helpers in `vo-module`
- move the shared extension ABI version constant to one authoritative source in `vo_runtime::ffi`
- update `vo-ext` and `vo-runtime::ext_loader` to use that single ABI constant source
- update `vo-engine`, `vo-vm`, `vo-stdlib`, `vo-web`, and other consumers to import full manifest types from `vo-module` and bridge to `NativeExtensionSpec` only at runtime load boundaries

### Expected file groups

- `lang/crates/vo-runtime/src/ext_loader.rs`
- `lang/crates/vo-runtime/src/ffi/`
- `lang/crates/vo-ext/src/lib.rs`
- `lang/crates/vo-engine/src/compile/`
- `lang/crates/vo-engine/src/run.rs`
- `lang/crates/vo-vm/src/vm/`
- `lang/crates/vo-stdlib/src/toolchain.rs`

### Acceptance criteria

- `vo-runtime` no longer depends on `vo-module`
- full extension manifest types are imported from `vo-module`, while `vo-runtime` loader code uses only `NativeExtensionSpec`
- no caller imports `ExtensionManifest` through `vo-runtime`
- ABI version is defined once and referenced everywhere else

---

## Phase 2 — Move Registry / Release Rules to Pure Shared Helpers

### Goal

Make `vo-module` the authoritative owner of release / manifest / locked-artifact rules while leaving transport platform-specific.

### Required changes

- extract pure helpers for:
  - release-tag construction
  - release asset URL calculation
  - requested-manifest identity validation
  - locked-manifest validation
  - bytes size / digest verification
  - conversion from installed release metadata to `LockedModule`
- keep native `GitHubRegistry` as the sync transport implementation
- keep `vo-web` browser fetch code as the async transport implementation
- delete the duplicated rule logic from `vo-web/src/module_install/fetch.rs` once the shared helpers exist

### Expected file groups

- `lang/crates/vo-module/src/registry.rs`
- new pure helper modules under `lang/crates/vo-module/src/`
- `lang/crates/vo-module/src/github_registry.rs`
- `lang/crates/vo-web/src/module_install/fetch.rs`
- `lang/crates/vo-web/src/module_install/mod.rs`

### Acceptance criteria

- `vo-web` retains browser-specific fetch code but no longer owns duplicated manifest / lock validation rules
- release-tag and asset URL rules are defined once
- manifest validation decisions match between native and web

---

## Phase 3 — Make Engine Extension State Explicit Across Compile and Run

### Goal

Remove duplicate extension preparation from the engine by making the compile-to-run contract explicit.

### Required changes

- introduce an engine-side prepared-extension type with explicit semantics
- make `CompileOutput.extensions` use that explicit prepared type instead of a discovery-manifest type
- ensure compile/check paths are the only owners of frozen-build extension preparation
- update `run.rs` and GUI VM construction to load prepared extensions directly
- remove the second prepare step from runtime loading

### Expected file groups

- `lang/crates/vo-engine/src/compile/mod.rs`
- `lang/crates/vo-engine/src/compile/pipeline.rs`
- `lang/crates/vo-engine/src/compile/native.rs`
- `lang/crates/vo-engine/src/run.rs`
- dependent toolchain / Studio-native consumers if types change publicly

### Acceptance criteria

- compile output clearly represents prepared, loadable extension state
- run-time loading performs no additional preparation pass
- one extension-preparation policy exists in engine code

---

## Phase 4 — Collapse `vo-web` Compile Internals Onto One Skeleton

### Goal

Keep the existing public convenience entry points while eliminating copy-structured internal compile flows.

### Required changes

- introduce one internal compile pipeline parameterized by source mode and module-source mode
- centralize `FileSet` construction, project-context loading, resolver building, analysis, and codegen sequencing
- preserve public APIs as thin wrappers only if they truly differ by caller convenience
- make workspace-replace handling and project-context loading go through the same internal flow everywhere possible

### Expected file groups

- `lang/crates/vo-web/src/compile.rs`
- possibly `lang/crates/vo-web/src/lib.rs` if public exports are renamed or narrowed

### Acceptance criteria

- semantic fixes to resolver / project-context handling touch one internal pipeline instead of four parallel ones
- all public compile entry points become thin wrappers over one shared implementation

---

## Phase 5 — Clean Transitional Public Surfaces

### Goal

Stop teaching new code to depend on transitional wrappers and removed install paths.

### Required changes

- migrate internal callers from `vo-module::compat` to typed identity / validation APIs where practical
- leave `compat` only as a narrow boundary helper for string-oriented consumers that genuinely need it
- stop publicly fronting `vo_module::project::ProjectContext` through `vo-web` unless a web-specific semantic wrapper is added
- move exact module-install behavior needed by toolchain integration onto an authoritative `vo-module` lifecycle or cache-install API
- delete or narrow engine-side removed install helpers once the authoritative path exists

### Expected file groups

- `lang/crates/vo-module/src/compat.rs`
- `lang/crates/vo-analysis/src/`
- `lang/crates/vo-web/src/lib.rs`
- `lang/crates/vo-web/src/compile.rs`
- `lang/crates/vo-engine/src/toolchain.rs`
- corresponding CLI / studio / tooling consumers

### Acceptance criteria

- typed APIs are the default internal path
- `vo-web` no longer appears to own shared project-context concepts
- toolchain exact-install behavior no longer preserves removed `vo get` architecture by accident

---

## Phase 6 — Final Contract Hardening and Cleanup

### Goal

Remove remaining small duplicated contracts only after the larger architectural moves land.

### Required changes

- review compile-log vs host-log schema overlap and either document intentional divergence or define a small shared record core
- delete temporary re-exports or temporary adapters introduced during the cleanup pass

### Acceptance criteria

- no conceptually central contract is defined twice just because it crosses native / web / runtime boundaries
- temporary cleanup shims are not left behind as permanent public surface

---

## 7. Implementation Order

The cleanup should land in this order:

1. Phase 1 — runtime/module extension-boundary decoupling
2. Phase 2 — registry / release rule extraction
3. Phase 3 — engine compile/run extension contract cleanup
4. Phase 4 — web internal compile skeleton unification
5. Phase 5 — public-surface cleanup
6. Phase 6 — final contract hardening

This order is important.
Phase 1 and Phase 2 stabilize the low-level boundary and shared rule ownership that make the later cleanup possible without inventing more transitional glue.

---

## 8. Non-Goals

This cleanup pass should **not** silently expand scope into unrelated redesigns.

Non-goals:

- changing module-system semantics already established by `vo.mod` / `vo.lock` / `vo.work`
- redesigning browser fetch transport itself
- redesigning the host capability bridge in `vo-ext`
- changing the user-facing CLI module model beyond removing architectural drift
- introducing fallback semantics that weaken current fail-fast behavior

---

## 9. Validation Plan

The exact commands may evolve with file moves, but the pass should at minimum be revalidated with:

```sh
cargo test -p vo-module --release
cargo test -p vo-engine --release
cargo test -p vo-web --release
cargo test -p vo-ext --release
cargo check -p vo --release
cargo check --release --target wasm32-unknown-unknown -p vo-web
cargo check --release --target wasm32-unknown-unknown --manifest-path studio/wasm/Cargo.toml
cargo check --manifest-path studio/src-tauri/Cargo.toml --release
```

Phase 1 changes to extension loader input and manifest ownership should also receive focused checks for:

- extension manifest parsing in `vo-module`
- platform-specific native library path resolution
- runtime loader construction from `NativeExtensionSpec`
- shared ABI version contract

---

## 10. End State

When this document is complete, the architecture should have these properties:

- `vo-module` owns extension manifest parsing and module-facing extension metadata
- `vo-runtime` owns only loader-facing native extension specs and runtime loading ABI checks
- `vo-module` owns pure module / release / lock rules regardless of transport backend
- `vo-engine` has one explicit compile-to-run extension state transition
- `vo-web` keeps browser-specific code without owning duplicated protocol rules
- thin wrappers no longer masquerade as authoritative public APIs

That is the cleanup bar for this pass.
