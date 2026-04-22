# Studio Browser Runtime Library API Plan

**Date**: 2026-04-20
**Status**: Proposed

## Context

The phase-2 Studio contract migration fixed the public frontend/native boundary:
`FrameworkContract` now uses a generic `jsModules` map plus optional `entry`
instead of hard-coded `rendererPath`, `protocolPath`, and `hostBridgePath`.

That migration was necessary, but it only solved the **public contract shape**.
It did **not** solve why `studio/src-tauri/src/commands/gui.rs` still contains a
large amount of browser-runtime-specific logic:

- manifest parsing and relative asset resolution
- runtime/framework selection policy
- renderer snapshot assembly
- WASM extension artifact preparation and freshness logic

The remaining problem is architectural: the library layers still expose
**fragments** of the truth, so Studio has to reconstruct the full execution model
locally.

## Problem Statement

Today Studio native has to do all of the following itself:

1. Parse extension manifests again and resolve relative asset paths.
2. Reconstruct browser runtime modules from manifest fragments.
3. Decide which framework is the "primary" one and which are providers.
4. Infer renderer snapshot mounts by walking directories derived from public
   runtime contract data.
5. Know development-time WASM artifact rules (`wasm-standalone`, `pkg-island`,
   local output conventions, freshness checks, sync behavior).

This is not because Studio is a special snowflake. It is because the libraries
currently provide:

- manifest schema and parsing helpers
- public browser runtime contract structs
- some VFS / WASM helpers

But they do **not** provide a complete chain from:

```text
manifest truth
  -> resolved extension truth
  -> browser runtime graph
  -> snapshot / artifact intent
  -> host materialization
```

So Studio is forced to become the place where these layers are stitched
back together.

## Core Diagnosis

The architectural bug is **not** that a few helpers live in the wrong crate.
The real bug is that the canonical data model stops too early.

The library stack currently exposes data at two incomplete levels:

- **parsed manifest structs**
- **public runtime contracts**

What is missing are the intermediate forms that a host actually needs:

- a resolved extension inventory with ownership and normalized asset references
- a runtime graph that separates public contract from host binding
- an explicit snapshot plan
- an explicit artifact intent / preparation plan

Because those layers are missing, Studio has to use the public contract as a
backdoor host-binding structure. That is the root cause of the current garbage
code.

## Design Goals

1. **Remove browser-runtime knowledge from Studio host code.**
2. **Keep the canonical runtime model generic.** Module roles come from
   `jsModules`; the library must not regress to special-casing renderer /
   protocol / host bridge.
3. **Separate public contract from host binding.**
4. **Make planning host-neutral.** Canonical plans must not depend on native
   absolute paths or `std::fs` assumptions.
5. **Let compile, run, and snapshot share one authority.**
6. **Keep `vo-web` core pure.** No process spawning or native-only execution
   logic in the core planning layer.
7. **Allow compatibility projections.** Existing Studio IPC may temporarily keep
   `primaryFramework` / `providerFrameworks`, but that must be a projection,
   not the canonical model.

## Non-Goals

1. Replacing Tauri command wrappers or Studio session state.
2. Moving process execution into `vo-web` core.
3. Encoding host absolute paths into public runtime contracts.
4. Making `primaryFramework` / `providerFrameworks` the canonical browser
   runtime model.
5. Solving every dev-tooling concern in the first step.

## Canonical Architecture

```text
extension manifests / ready modules
  -> ResolvedExtensionSet            (vo-module)
  -> BrowserRuntimeGraph             (vo-web core)
  -> BrowserRuntimeView              (public projection)
  -> BrowserSnapshotPlan             (vo-web core)
  -> BrowserArtifactIntent           (vo-web core)
  -> BrowserArtifactPlan             (dev-oriented layer)
  -> host execution / materialization (Studio native, Studio wasm, future hosts)
```

The important change is that the library stack owns the **truth chain** all the
way to explicit plans. Studio should only own:

- session/cache lifetime
- IPC boundaries
- UX/logging
- actual process execution and filesystem materialization glue

## Key Design Rule: Public Contract vs Host Binding

This is the most important rule in the whole design.

A browser framework must have **two different representations**:

### Public view

This is what frontend code or IPC boundaries consume:

```rust
pub struct BrowserRuntimeContract {
    pub name: String,
    pub entry: Option<String>,
    pub capabilities: Vec<String>,
    pub js_modules: BTreeMap<String, String>,
}
```

This is intentionally small and stable.

### Host binding view

This is what planners and hosts consume internally:

```rust
pub struct BrowserFrameworkBinding {
    pub framework_id: BrowserFrameworkId,
    pub owner: ExtensionOwner,
    pub module_assets: BTreeMap<String, AssetRef>,
    pub runtime_roots: Vec<AssetRootRef>,
    pub extra_artifacts: Vec<AssetRef>,
}
```

The exact shape can evolve, but the principle must stay:

- `BrowserRuntimeContract` is a public interface.
- `BrowserFrameworkBinding` is execution/planning data.

Studio becomes messy when it uses the public contract to rediscover host
bindings.

## `vo-module`: Resolved Extension Truth

`vo-module` should stop at more than raw parsing. It should expose a resolved,
host-neutral extension inventory.

### Proposed layer

```rust
pub struct ResolvedExtensionSet {
    pub extensions: Vec<ResolvedExtension>,
}

pub struct ResolvedExtension {
    pub id: ExtensionId,
    pub owner: ExtensionOwner,
    pub manifest: ExtensionManifest,
    pub web: Option<ResolvedWebRuntimeManifest>,
    pub wasm: Option<ResolvedWasmExtensionManifest>,
}
```

### Supporting type: `AssetRef`

The key new primitive is a logical asset reference:

```rust
pub struct AssetRef {
    pub owner: ExtensionOwner,
    pub relative_path: Utf8PathBuf,
}
```

The exact type is less important than the semantics:

- it identifies an asset relative to its owning module/extension
- it is **not** a host absolute path
- it can be resolved by native FS adapters or wasm/VFS adapters later

### Responsibilities of `vo-module`

`vo-module` should own:

- manifest schema and removed compatibility
- relative-path resolution rules
- ownership assignment for resolved assets
- conversion from parsed manifests to resolved web/wasm extension specs

This means Studio should not need local helpers like:

- `resolve_manifest_asset_path`
- `parse_browser_runtime_manifest`
- `parse_wasm_ext_manifest`

Those are not Studio concerns. They are part of manifest truth.

## `vo-web` Core: Browser Runtime Graph

The next layer is the canonical browser runtime model.

### Important correction: `primary/provider` is not the canonical model

The canonical model should **not** be:

- one primary framework
- zero or more provider frameworks

That is a compatibility projection for current Studio APIs.

The canonical model should instead be:

- a set of frameworks
- an index of which framework provides which JS module roles
- explicit entry ownership

### Proposed layer

```rust
pub struct BrowserRuntimeGraph {
    pub frameworks: Vec<BrowserFrameworkPlan>,
    pub roles: BrowserRoleIndex,
}

pub struct BrowserFrameworkPlan {
    pub id: BrowserFrameworkId,
    pub contract: BrowserRuntimeContract,
    pub binding: BrowserFrameworkBinding,
}

pub struct BrowserRoleIndex {
    pub entry_framework: Option<BrowserFrameworkId>,
    pub module_providers: BTreeMap<String, Vec<BrowserFrameworkId>>,
}
```

### Why this is better

1. It respects the new generic `jsModules` shape.
2. It avoids hard-coding renderer/protocol/host-bridge semantics in the core
   model.
3. It makes "who provides which role" explicit.
4. It lets Studio keep old `primaryFramework` / `providerFrameworks` as a helper
   projection if needed.

### Compatibility helper

If the current Studio IPC still needs the old split, expose it as a helper:

```rust
pub fn split_primary_provider_view(
    graph: &BrowserRuntimeGraph,
) -> RemovedFrameworkSplit
```

That helper is allowed to exist, but it must be clearly marked as a temporary
projection. It must not become the core runtime truth.

## `vo-web` Core: Browser Runtime View

Frontend and IPC layers still need a stable public payload. That should be a
view derived from `BrowserRuntimeGraph`, not an independently reconstructed
structure.

```rust
pub struct BrowserRuntimeView {
    pub frameworks: Vec<BrowserRuntimeContract>,
    pub roles: BrowserRoleIndexView,
}
```

Possible compatibility adapters:

- `removed_primary_framework()`
- `removed_provider_frameworks()`
- `removed_framework_contracts()`

The important rule is:

**All public views are projections from the runtime graph.**

Studio should never assemble framework contracts manually again.

## `vo-web` Core: Browser Snapshot Plan

The renderer bridge snapshot currently gets rebuilt by host code that walks
source trees and extension artifact directories. That should become a real plan.

### Proposed layer

```rust
pub struct BrowserSnapshotPlan {
    pub mounts: Vec<SnapshotMount>,
}

pub struct SnapshotMount {
    pub source: SnapshotSourceRef,
    pub virtual_prefix: Utf8PathBuf,
    pub strip_prefix: Option<Utf8PathBuf>,
    pub kind: SnapshotMountKind,
}
```

Again, the exact names may change, but the semantics are important.

### Design rules

1. `BrowserSnapshotPlan` describes **what must be mounted**, not the final file
   list.
2. Snapshot planning must derive from **bindings and artifact intent**, not from
   public contract strings.
3. If a virtual prefix like `wasm/` is required, it must be visible in the
   plan. It must not be an implicit Studio-local trick.
4. The same snapshot plan should be materializable from native FS and from VFS.

### Materialization

Materializers can live in host-specific or std/dev helpers:

- `materialize_snapshot_from_fs(plan, resolver, root)`
- `materialize_snapshot_from_vfs(plan, resolver, root)`

The planner owns semantics; the materializer owns IO.

## Artifact Handling: Intent First, Execution Later

The current Studio native code knows too much about development-time browser
artifacts:

- `wasm-standalone`
- `pkg-island`
- output directories
- freshness checks
- sync/copy rules

This is too much browser-runtime knowledge for Studio, but it should also **not**
all move into `vo-web` core, because process spawning and native filesystem work
are not core browser-runtime semantics.

So the design should split this into two layers.

### `vo-web` core: logical artifact intent

```rust
pub struct BrowserArtifactIntent {
    pub required_artifacts: Vec<RequiredBrowserArtifact>,
}
```

This answers:

- what artifacts the runtime logically requires
- which framework/extension owns them
- which logical roles they satisfy

### Dev-oriented layer: executable artifact plan

A dev-oriented layer (either a separate crate like `vo-web-dev` or a dev/std
submodule outside the core planning API) can then translate intent into
host-executable actions:

```rust
pub struct BrowserArtifactPlan {
    pub actions: Vec<ArtifactActionSpec>,
}
```

Potential action families:

- `EnsureStandaloneWasm`
- `EnsurePkgIsland`
- `SyncDeclaredArtifact`

### Important rule

`vo-web` core may define the data model for artifact intent, but the actual
process execution should stay outside the core layer.

That keeps the architecture clean:

- core layer = declarative and host-neutral
- dev layer = process execution, filesystem effects, local toolchain details

## Crate Responsibilities

### `vo-module`

Owns:

- manifest schema
- removed compatibility
- resolved extension truth
- asset ownership and logical asset references

### `vo-web` core

Owns:

- browser runtime graph
- role index
- public runtime view projections
- snapshot planning semantics
- logical artifact intent

### Dev-oriented helper layer

Owns:

- turning artifact intent into executable steps
- optional freshness logic helpers
- optional filesystem snapshot materializers
- optional process execution helpers

This layer must not redefine runtime semantics. It only executes or materializes
plans produced by the canonical layers.

### Studio

Owns:

- session state and caching
- IPC wrappers
- calling planners/materializers
- user-facing logging and diagnostics
- executing dev-time artifact plans in native mode

Studio should not be the authority on browser runtime semantics.

## How Studio Changes

With the proposed layers, Studio native would become much thinner.

### Run path

1. Compile / ready modules.
2. Build `ResolvedExtensionSet` from library APIs.
3. Build `BrowserRuntimeGraph` from `ResolvedExtensionSet`.
4. Derive `BrowserRuntimeView` for IPC/frontend.
5. Derive `BrowserArtifactIntent`.
6. In native dev mode, translate intent into `BrowserArtifactPlan` and execute it.
7. Cache the resulting runtime graph / snapshot plan / artifact results in
   session state.

### Snapshot path

1. Reuse the cached `BrowserSnapshotPlan` from session state.
2. Materialize it through FS or VFS helpers.
3. Return the file list.

The snapshot path should not:

- reparse manifests
- rediscover framework directories
- deduplicate extensions again
- reconstruct virtual prefixes again

## Code That Should Disappear from Studio

Once the library layers exist, the following Studio-native logic should no
longer be necessary as local truth holders:

- `resolve_manifest_asset_path`
- `parse_browser_runtime_manifest`
- `parse_wasm_ext_manifest`
- `extract_framework_contracts`
- `split_primary_framework_contract`
- most of the directory-derivation logic inside
  `cmd_get_renderer_bridge_vfs_snapshot`

Later, once artifact intent/plan lands, Studio should also be able to shed:

- `prepare_wasm_extensions`
- `ensure_standalone_wasm_ext_fresh`
- `ensure_pkg_island_fresh`
- local build-candidate / output-path / sync helper logic

## Hard Constraints

These constraints should be treated as design invariants.

### 1. No public-contract back-propagation

Do not infer snapshot or artifact plans from public `jsModules` strings.
Always derive them from bindings and resolved assets.

### 2. No host absolute paths in canonical planning APIs

Canonical planning data must stay host-neutral. Native hosts can resolve to
`PathBuf`; wasm hosts can resolve to VFS handles. The planner should not care.

### 3. No process execution in `vo-web` core

Planning belongs in the core layer. Filesystem side effects and command
execution do not.

### 4. No Studio-only semantics in the core runtime graph

If a concept only exists because current Studio IPC wants it, keep it as a
projection/helper, not as the core truth.

### 5. One runtime plan per session

Compile, run, and snapshot must all reuse the same runtime authority. The host
must not recompute runtime meaning separately in multiple commands.

## Recommended Migration Order

### Stage 1: Introduce resolved extension truth

Add `AssetRef`, `ResolvedExtensionSet`, `ResolvedWebRuntimeManifest`, and
`ResolvedWasmExtensionManifest` in `vo-module`.

**Immediate effect**:

- Studio no longer needs local manifest path resolution helpers.

### Stage 2: Introduce browser runtime graph + public view projection

Add `BrowserRuntimeGraph`, `BrowserFrameworkPlan`, `BrowserRoleIndex`, and
`BrowserRuntimeView` in `vo-web`.

**Immediate effect**:

- Studio no longer assembles framework contracts itself.
- `primary/provider` becomes a compatibility helper instead of a core concept.

### Stage 3: Introduce browser snapshot planning

Add `BrowserSnapshotPlan` plus snapshot materialization helpers.

**Immediate effect**:

- `cmd_get_renderer_bridge_vfs_snapshot` becomes thin and deterministic.
- virtual mount semantics become explicit.

### Stage 4: Introduce artifact intent and dev-oriented artifact planning

Add `BrowserArtifactIntent` in the core layer and `BrowserArtifactPlan` in a
dev-oriented layer.

**Immediate effect**:

- Studio stops being the authority on local browser artifact preparation.
- build orchestration becomes explicit instead of hidden in read-like helpers.

## Why This Is the Elegant Direction

The goal is not to make Studio smarter. The goal is to make the library layers
complete enough that Studio can become boring.

A good host should only have to:

- ask the library for truth
- ask the library for plans
- execute/materialize those plans
- expose the result over its own session and IPC boundaries

That is the clean boundary.

## Final Summary

The remaining Studio-native browser-runtime garbage code exists because the
current library stack stops at parsed manifests and public contracts.

The correct fix is to extend the canonical stack with:

- resolved extension truth (`vo-module`)
- runtime graph (`vo-web` core)
- public runtime view projection (`vo-web` core)
- snapshot planning (`vo-web` core)
- artifact intent (`vo-web` core)
- dev-oriented executable artifact planning (outside `vo-web` core)

Once those layers exist, Studio can delete most of its local browser-runtime
reasoning and shrink back to what it should be: a host shell, not the owner of
browser runtime semantics.
