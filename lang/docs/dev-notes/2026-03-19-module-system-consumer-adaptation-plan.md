# Module System Consumer Adaptation Plan

**Date**: 2026-03-19
**Status**: In Progress
**Scope**: Adapt all consumer crates to the new `vo-module-new` crate
**Prerequisite**: `vo-module-new` crate complete (Phases 1–8 done, 91 tests passing)
**Related**: `2026-03-19-module-system-rewrite-plan.md` (Phase 9)

---

## 1. Current State

The new `vo-module-new` crate is complete at `lang/crates/vo-module/` with:

- **Layer 1 (core)**: `ModulePath`, `ExactVersion`, `ToolchainVersion`, `DepConstraint`, `ToolchainConstraint`, `Digest`, `ArtifactId`, import classification, internal visibility, `find_owning_module`
- **Layer 2 (schema)**: `ModFile`, `LockFile`, `WorkFile`, `ReleaseManifest` — parse, validate, render
- **Layer 3 (registry)**: `Registry` trait, repo mapping, tag computation, manifest validation
- **Layer 4 (solver)**: deterministic highest-version solver with targeted update preferences
- **Layer 5 (lock)**: lock generation, root consistency, module-vs-manifest verification, graph completeness
- **Layer 6 (materialize)**: download + digest verify, cache model, frozen-build verification
- **Layer 7 (workspace)**: `vo.work` discovery, override resolution, identity check, self-override check
- **Layer 8 (ops)**: `mod_init`, `mod_add`, `mod_update`, `mod_sync`, `mod_download`, `mod_verify`, `mod_remove`, `frozen_build_check`, `resolve_import_owner`
- **ext_manifest**: `ExtensionManifest`, `discover_extensions`, `WasmExtensionManifest`
- **compat**: `validate_import_path`, `validate_internal_access`, `is_valid_module_path`, `module_repository`, `module_root`

The old crate lives at `lang/crates/vo-module-old/` (package name still `vo-module`). All consumers currently depend on the old crate via `path = "../vo-module-old"`.

---

## 2. Key API Differences Between Old and New

Understanding these differences is essential for planning the adaptation.

### 2.1 Type System

| Old | New | Impact |
|---|---|---|
| `ModFile.module: String` | `ModFile.module: ModulePath` | All consumers that read module path as string need `.as_str()` |
| `ModFile.requires: Vec<Require>` where `Require.module: String` | `ModFile.require: Vec<Require>` where `Require.module: ModulePath` | Field name change + typed module path |
| `LockFile` with `verify_graph_consistency()` method | Separate `lock::verify_root_consistency()` + `lock::verify_graph_completeness()` functions | Call sites change |
| `LockFile::new(module, vo, created_by)` constructor | `lock::generate_lock(mod_file, graph, created_by)` | Lock generation is now solver-output-driven |
| `LockFile::parse(&content, &path)` | `LockFile::parse(&content)` (path not needed) | Simpler API |
| `LockedModule` with string fields + serde Serialize/Deserialize | `LockedModule` with typed fields, no serde | Engine cache serialization needs adapter |
| `ModuleError` enum | `Error` enum | Different variant names |

### 2.2 Functionality Location

| Old location | New location | Notes |
|---|---|---|
| `vo_module::validate_import_path` | `vo_module_new::compat::validate_import_path` | Same signature |
| `vo_module::validate_internal_access` | `vo_module_new::compat::validate_internal_access` | Same signature |
| `vo_module::is_valid_module_path` | `vo_module_new::compat::is_valid_module_path` | Same signature |
| `vo_module::module_repository` | `vo_module_new::compat::module_repository` | Returns `ModuleRepository` instead of old type |
| `vo_module::find_workspace_replaces` | `vo_module_new::workspace::discover_workfile` + `resolve_overrides` | Two-step API |
| `vo_module::ModFile::parse(&content, &path)` | `vo_module_new::schema::modfile::ModFile::parse(&content)` | Path arg removed |
| `vo_module::LockFile::parse(&content, &path)` | `vo_module_new::schema::lockfile::LockFile::parse(&content)` | Path arg removed |
| `vo_module::fetch::module_cache_dir` | `vo_module_new::materialize::cache_dir` | Different signature |
| `vo_module::fetch::install_module` | `vo_module_new::materialize::download_source` | Different API |
| `vo_module::fetch::validate_locked_module_for_frozen_build` | `vo_module_new::materialize::verify_frozen_cache` | Whole-graph instead of per-module |
| `vo_module::fetch::validate_extension_manifests_for_frozen_build` | Removed from module system | Extension validation belongs in vo-engine |
| `vo_module::lifecycle::*` | `vo_module_new::ops::*` | Different function signatures |
| `vo_module::ExtensionManifest` | `vo_module_new::ext_manifest::ExtensionManifest` | Same structure |
| `vo_module::discover_extensions` | `vo_module_new::ext_manifest::discover_extensions` | Same signature |

### 2.3 Removed from Module System (must be handled by consumers)

| Functionality | Why removed | Where it goes |
|---|---|---|
| VFS resolver (`Resolver` trait, `StdSource`, `ModSource`, `PackageResolverMixed`, `ReplacingResolver`, `CurrentModuleResolver`) | Package resolution from filesystem is a compiler concern, not a module protocol concern. The module system defines *ownership* (spec §9.1), the compiler implements *file reading*. | `vo-analysis` |
| WASM async fetch (`fetch::wasm::*`) | WASM network layer is a runtime concern | `vo-web` (already there) |
| `fetch::validate_extension_manifests_for_frozen_build` | Extension build policy is an engine concern | `vo-engine` |
| `fetch::ensure_extension_manifests_built` | Extension build orchestration is an engine concern | `vo-engine` |

---

## 3. Adaptation Steps — Ordered by Dependency

### Step 1: vo-ffi-macro

**Effort**: None — grep shows no `vo_module` usage in source files. The Cargo.toml dependency exists but is unused.

**Action**: Remove `vo-module` from `vo-ffi-macro/Cargo.toml`.

**Verification**: `cargo check -p vo-ffi-macro`

---

### Step 2: vo-runtime

**Effort**: Minimal — 1 file, 1 line.

**Current**: `pub use vo_module::{ExtensionManifest, discover_extensions};` in `ext_loader.rs`

**Action**: Change to `pub use vo_module_new::ext_manifest::{ExtensionManifest, discover_extensions};`

**Cargo.toml**: Replace `vo-module` dependency with `vo-module-new`.

**Verification**: `cargo check -p vo-runtime`

---

### Step 3: vo-stdlib

**Effort**: Small — 1 file (`toolchain.rs`), uses `vo_module::LockedModule` and `vo_module::fetch::install_module`.

**Current usage**:
```rust
use vo_module::LockedModule;
vo_module::fetch::install_module(module, version)
```

**Action**:
- Replace `LockedModule` with `vo_module_new::schema::lockfile::LockedModule`
- Replace `fetch::install_module` — this function downloads a module to cache. The new equivalent is `materialize::download_source` but it requires a `Registry` instance and a `LockedModule`. The stdlib toolchain module likely needs a simpler wrapper.
- May need to add a convenience function to `vo_module_new::ops` or keep a thin local wrapper.

**Verification**: `cargo check -p vo-stdlib --features std`

---

### Step 4: vo-analysis — VFS Resolver Migration

**Effort**: Large — this is the biggest structural change.

**Current**: `vo-analysis` imports `Resolver`, `discover_extensions`, `ExtensionManifest`, `validate_import_path`, `validate_internal_access`, `is_valid_module_path`, `VfsPackage`, and the full VFS type hierarchy from `vo_module`.

**Action — Part A (identity/validation)**:
- Replace `vo_module::validate_import_path(path)` with `vo_module_new::compat::validate_import_path(path)` — same signature
- Replace `vo_module::validate_internal_access(a, b)` with `vo_module_new::compat::validate_internal_access(a, b)` — same signature
- Replace `vo_module::is_valid_module_path(p)` with `vo_module_new::compat::is_valid_module_path(p)` — same signature
- Replace `vo_module::ExtensionManifest` with `vo_module_new::ext_manifest::ExtensionManifest`
- Replace `vo_module::discover_extensions` with `vo_module_new::ext_manifest::discover_extensions`

**Action — Part B (VFS resolver)**:
- Copy the VFS resolver code (`Resolver` trait, `StdSource`, `ModSource`, `PackageResolverMixed`, `ReplacingResolver`, `CurrentModuleResolver`, `VfsPackage`, `VfsFile`) from `vo-module-old/src/vfs.rs` into a new `vo-analysis/src/vfs.rs` module.
- This code depends on `vo_common::vfs::FileSystem` which `vo-analysis` already uses.
- The VFS resolver code also uses `ext_manifest::extension_name_from_content` — import from new crate.
- The `ModSource` and related types use `vo_common::abi::package_abi_path` — `vo-analysis` already depends on `vo-common`.
- Update all internal references within `vo-analysis` to use the local VFS module.

**Files to modify**:
- `vo-analysis/Cargo.toml` — add `vo-module-new`, remove `vo-module`
- `vo-analysis/src/vfs.rs` — new file (migrated from `vo-module-old/src/vfs.rs`)
- `vo-analysis/src/lib.rs` — register `vfs` module
- `vo-analysis/src/importer.rs` — update imports
- `vo-analysis/src/project.rs` — update imports

**Verification**: `cargo test -p vo-analysis --release`

---

### Step 5: vo-release

**Effort**: Medium — uses `ModFile`, `LockFile`, `LockedModule`, `ReleaseManifest`, `ModuleError`, `lifecycle::verify_dependency_graph`.

**Current usage** (3 files):
- `error.rs`: `use vo_module::ModuleError`
- `repo.rs`: `use vo_module::{ModFile, ReleaseArtifact, ReleaseManifest, ...}`, calls `vo_module::lifecycle::verify_dependency_graph`
- `tests.rs`: `use vo_module::{LockFile, LockedModule, ModuleError, ReleaseManifest}`

**Action**:
- Replace `ModuleError` with `vo_module_new::Error`
- Replace `ModFile` with `vo_module_new::schema::modfile::ModFile`
- Replace `LockFile` with `vo_module_new::schema::lockfile::LockFile`
- Replace `LockedModule` with `vo_module_new::schema::lockfile::LockedModule`
- Replace `ReleaseManifest` with `vo_module_new::schema::manifest::ReleaseManifest`
- Replace `ReleaseArtifact` with `vo_module_new::schema::manifest::ManifestArtifact`
- Replace `lifecycle::verify_dependency_graph` with `lock::verify_root_consistency` + `lock::verify_graph_completeness`
- Adapt field access patterns (`.module` → `.module.as_str()`, `.requires` → `.require`, etc.)

**Verification**: `cargo test -p vo-release --release`

---

### Step 6: vo-engine

**Effort**: Very Large — `compile.rs` (961 lines) is the heaviest consumer.

**Current usage**: `ModFile`, `LockFile`, `LockedModule`, `ModSource`, `StdSource`, `PackageResolverMixed`, `ReplacingResolver`, `CurrentModuleResolver`, `module_repository`, `find_workspace_replaces`, `fetch::*` (6+ functions), `discover_extensions`.

**Action — Part A (types)**:
- Replace all `vo_module::ModFile` → `vo_module_new::schema::modfile::ModFile`
- Replace all `vo_module::LockFile` → `vo_module_new::schema::lockfile::LockFile`
- Replace all `vo_module::LockedModule` → `vo_module_new::schema::lockfile::LockedModule`
- Add serde support to new `LockedModule` (it's serialized to compile cache files) — either add `Serialize`/`Deserialize` derives or create a local serializable wrapper in vo-engine

**Action — Part B (VFS resolver)**:
- After Step 4 moves VFS resolver to `vo-analysis`, `vo-engine` imports it from `vo-analysis` instead of `vo-module`
- `vo-engine` already depends on `vo-analysis`

**Action — Part C (workspace)**:
- Replace `vo_module::find_workspace_replaces(root)` with:
  ```rust
  let workfile_path = vo_module_new::workspace::discover_workfile(root);
  // ... parse and resolve overrides
  ```
- Or add a convenience wrapper in `vo_module_new::workspace` that returns `HashMap<String, PathBuf>` like the old API

**Action — Part D (fetch/cache)**:
- Replace `vo_module::fetch::module_cache_dir` → `vo_module_new::materialize::cache_dir`
- Replace `vo_module::fetch::locked_module_cache_dir` → local helper using `materialize::cache_dir`
- Replace `vo_module::fetch::validate_installed_module` → `vo_module_new::materialize::verify_frozen_cache` (whole-graph)
- Replace `vo_module::fetch::validate_extension_manifests_for_frozen_build` → local implementation in vo-engine (this is engine policy, not module protocol)

**Action — Part E (module plan)**:
- Replace `read_external_module_plan` to use new `ModFile::parse`, `LockFile::parse`, `lock::verify_root_consistency`, `lock::verify_graph_completeness`
- The old `verify_graph_consistency` returned `Vec<LockedModule>` — the new lock verification returns `Result<(), Error>` and the locked modules are already in `lock_file.resolved`

**Action — Part F (tests)**:
- Update test helpers that construct `LockFile::new(...)` and `LockedModule` — use new constructors
- Update test assertions for new field types

**Files to modify**:
- `vo-engine/Cargo.toml` — add `vo-module-new`, remove `vo-module`
- `vo-engine/src/compile.rs` — extensive changes
- `vo-engine/src/run.rs` — update `LockedModule` type, `fetch::ensure_extension_manifests_built`

**Verification**: `cargo test -p vo-engine --release`

---

### Step 7: cmd/vo

**Effort**: Medium — `main.rs` (723 lines) uses lifecycle commands.

**Current usage**:
- `vo_module::lifecycle::*` for mod add/update/sync/download/remove
- `vo_module::ModFile::parse` for init validation
- `vo_module::LockFile` for release verification

**Action**:
- Replace all lifecycle calls with `vo_module_new::ops::*`
- The new ops layer has the same semantics but different function signatures
- `ops::mod_add(project_dir, dep_path, constraint, registry, created_by)` replaces the old lifecycle function
- Need to construct a `Registry` impl — add `vo_module_new::registry::GitHubRegistry` (currently only the trait exists; native impl needs to be added)

**Prerequisite**: Need to implement `GitHubRegistry` struct in `vo_module_new::registry` that uses `ureq` for HTTP

**Files to modify**:
- `cmd/vo/Cargo.toml` — add `vo-module-new`, remove `vo-module`
- `cmd/vo/src/main.rs` — update all `cmd_mod` functions

**Verification**: `cargo test -p vo --release`

---

### Step 8: vo-web

**Effort**: Medium — WASM fetch stays in `vo-web`, but type references change.

**Current usage**:
- `vo_module::ModFile::parse` — for project compilation
- `vo_module::LockFile::parse` — for dependency verification
- `vo_module::LockedModule` — for locked module fetch
- `vo_module::fetch::wasm::*` — WASM async fetch functions (these are defined in the old crate's `fetch.rs` behind `#[cfg(target_arch = "wasm32")]`)

**Action — Part A (types)**:
- Replace `ModFile`/`LockFile`/`LockedModule` imports with new crate types

**Action — Part B (WASM fetch)**:
- The WASM fetch functions (`fetch_module_files`, `fetch_locked_module_files`, `fetch_wasm_binary`, etc.) are currently in `vo-module-old/src/fetch.rs` under `mod wasm`. They need to be moved into `vo-web/src/module_install.rs` (which already exists and wraps them).
- These functions use internal helpers from the old fetch module (`release_manifest_url`, `sha256_digest`, `verify_download_bytes`, `extract_release_source_package_files`). These helpers need to either be reimplemented locally in `vo-web` or exposed from the new crate's `materialize` module.

**Files to modify**:
- `vo-web/Cargo.toml` — add `vo-module-new`, remove `vo-module`
- `vo-web/src/lib.rs` — update imports
- `vo-web/src/module_install.rs` — update types + potentially inline WASM fetch helpers

**Verification**: `cargo check -p vo-web --target wasm32-unknown-unknown`

---

### Step 9: playground/rust and studio

**Effort**: Small — mostly Cargo.toml changes + a few type imports.

**playground/rust**: Uses `vo_module` for `LockedModule` type in a few places.
**studio/src-tauri**: Uses `vo_module` for state management types.
**studio/wasm**: Uses `vo_module` for locked module types.

**Action**: Update Cargo.toml dependencies and fix type imports.

**Verification**:
- `cargo check -p vo-playground`
- `cargo check --release --target wasm32-unknown-unknown` in `studio/wasm`

---

### Step 10: GitHubRegistry Implementation

**Effort**: Medium — need a concrete `Registry` trait implementation using `ureq`.

This is a prerequisite for Step 7 (cmd/vo) but can be done in parallel with earlier steps.

**Action**: Implement `pub struct GitHubRegistry` in `vo_module_new::registry` that:
- Enumerates Git tags via GitHub API
- Filters to valid published versions (tag + release + manifest + source)
- Fetches `vo.release.json` from GitHub Release assets
- Fetches source packages and artifacts from GitHub Release assets
- All behind `#[cfg(not(target_arch = "wasm32"))]`

**Files to modify**:
- `vo-module/src/registry.rs` — add `GitHubRegistry` struct

**Verification**: `cargo check -p vo-module-new`

---

### Step 11: Add Serde Support to Lock Types

**Effort**: Small — `vo-engine` serializes `LockedModule` to compile cache files.

**Action**: Add `#[derive(Serialize, Deserialize)]` to:
- `LockedModule`
- `LockedArtifact`
- `LockFile` (maybe, for cache)

Or alternatively, create a serializable wrapper in `vo-engine` that converts between new typed fields and plain strings.

**Decision**: Adding serde to the schema types is clean — they already live in the schema layer which uses serde for `ReleaseManifest`. The core layer stays serde-free.

**Files to modify**:
- `vo-module/src/schema/lockfile.rs` — add serde derives
- `vo-module/src/core.rs` — add serde derives to types used in lockfile (ExactVersion, ModulePath, ToolchainConstraint, Digest, ArtifactId)

**Verification**: `cargo test -p vo-module-new --release`

---

### Step 12: Final Rename

**Effort**: Small — rename `vo-module-new` to `vo-module`.

**Action**:
- Change `name = "vo-module-new"` to `name = "vo-module"` in `lang/crates/vo-module/Cargo.toml`
- Update all consumer Cargo.toml that reference `vo-module-new` to reference `vo-module`
- Update workspace patch in root `Cargo.toml`
- User deletes `vo-module-old` directory

**Verification**: Full workspace build

---

## 4. Recommended Execution Order

```text
Step 10: GitHubRegistry implementation (unblocks cmd/vo)
Step 11: Add serde to lock types (unblocks vo-engine cache)
    │
Step 1: vo-ffi-macro (trivial, removes unused dep)
Step 2: vo-runtime (1-line change)
Step 3: vo-stdlib (small)
    │
Step 4: vo-analysis (VFS migration — largest structural change)
    │
Step 5: vo-release (medium)
Step 6: vo-engine (very large — depends on Step 4 for VFS)
Step 7: cmd/vo (depends on Step 10 for GitHubRegistry)
Step 8: vo-web (medium)
    │
Step 9: playground/studio (small)
    │
Step 12: Final rename (after user deletes vo-module-old)
```

Steps 1–3 can be done immediately. Steps 10–11 can be done in parallel with Steps 1–3.

---

## 5. Workspace Convenience Functions to Add

During adaptation, some consumers need convenience wrappers that the new crate doesn't yet provide. These should be added to the new crate before consumer adaptation begins:

### 5.1 `workspace::find_workspace_replaces`

Old signature: `fn find_workspace_replaces(root: &Path) -> Result<HashMap<String, PathBuf>, ModuleError>`

New equivalent needed:
```rust
pub fn find_workspace_replaces(root: &Path) -> Result<HashMap<String, PathBuf>, Error> {
    // discover vo.work, resolve overrides, return module->dir map
}
```

### 5.2 `materialize::locked_module_cache_dir`

Old: `fn locked_module_cache_dir(mod_root: &Path, locked: &LockedModule) -> PathBuf`

Already exists as `cache_dir` but with different signature. Add a convenience wrapper.

### 5.3 `materialize::validate_installed_module`

Old: per-module validation. New: whole-graph validation. May need per-module variant for engine.

---

## 6. Risk Assessment

| Risk | Impact | Mitigation |
|---|---|---|
| VFS migration to vo-analysis introduces compile errors across vo-engine | High | Do Step 4 and Step 6 together as one atomic change |
| LockedModule serde change breaks compile cache compatibility | Low | Cache files are ephemeral; stale cache is rebuilt automatically |
| GitHubRegistry implementation takes longer than expected | Medium | cmd/vo can temporarily keep a thin wrapper around old lifecycle |
| WASM fetch migration to vo-web is complex | Medium | WASM fetch can stay behind old crate temporarily while other consumers migrate first |

---

## 7. Validation Checklist

After all steps are complete:

```text
cargo test -p vo-module-new --release
cargo test -p vo-analysis --release
cargo test -p vo-engine --release
cargo test -p vo-web --release
cargo test -p vo --release
cargo check -p vo --release
cargo check -p vo-module-new --release --target wasm32-unknown-unknown
cargo check -p vo-web --release --target wasm32-unknown-unknown
cargo check --release --target wasm32-unknown-unknown    # playground/rust
cargo check --release --target wasm32-unknown-unknown    # studio/wasm
```
