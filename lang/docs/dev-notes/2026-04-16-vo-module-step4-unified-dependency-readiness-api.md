# Step 4: Unified Dependency Readiness API

**Date**: 2026-04-16
**Status**: Planning
**Parent**: `2026-04-15-vo-module-final-form-master-plan.md` — Step 4
**Depends on**: Step 3 (target-artifact resolution API)
**Normative reference**: `lang/docs/spec/module.md` §8.1–8.4

---

## 1. Objective

Create a single `vo-module` entry point that answers: "are all dependencies for this project ready for a frozen build on this target?" Replace the scattered readiness pipelines in `vo-engine` (compile/native) and `vo-web` (module_install) with calls to this unified API.

After this step:
- `vo-module` owns the complete frozen-build readiness contract: source installed, integrity verified, required target artifacts present and valid.
- `vo-engine` calls one function instead of assembling its own validation chain from `validate_locked_modules_installed` + `prepare_native_extension_specs_for_frozen_build`.
- `vo-web` can call the same function with `wasm32-unknown-unknown` to verify VFS module readiness.
- Studio and `vo-app-runtime` never need to understand dependency preparation details.

---

## 2. Current State

### 2.1 `vo-engine` readiness pipeline

The compile pipeline (`pipeline.rs`) currently orchestrates readiness in two phases:

1. **`validate_locked_modules_installed`** (`native.rs:15–23`)
   - Iterates all `LockedModule` entries
   - Calls `vo_module::cache::validate::validate_installed_module` for each
   - Verifies: directory exists, `vo.mod` matches, `.vo-version` matches, `.vo-source-digest` matches, `vo.release.json` digest matches
   - Does NOT check artifact presence or integrity

2. **`prepare_native_extension_specs_for_frozen_build`** (`native.rs:64–106`)
   - Iterates `ExtensionManifest` entries that have `native`
   - For cached modules: finds `LockedModule`, validates installed module again, finds locked native artifact, validates artifact bytes, returns `NativeExtensionSpec`
   - For local modules: checks if native lib is built, optionally triggers cargo build

**Problem**: These two phases are split across different call sites, they duplicate validation, and artifact readiness is tangled with `NativeExtensionSpec` construction (a runtime concern).

### 2.2 `vo-web` readiness pipeline

`vo-web/src/module_install/mod.rs` has a different readiness model:

- `validate_vfs_installed_module` — checks VFS cache markers
- `ensure_vfs_locked_modules` — for each locked module, either validate-and-load-ext or install-and-load-ext
- Artifact readiness is implicit: if the wasm extension file exists in VFS, it's "ready"

**Problem**: No integrity verification of wasm artifacts against lock digests. No unified "is this module ready for target X?" question.

### 2.3 `vo-module` existing primitives

- `cache::validate::validate_installed_module(fs, module_dir, locked)` — source-level validation
- `cache::validate::validate_installed_artifact(fs, artifact_path, locked_module, artifact)` — single artifact validation
- `lifecycle::locked_module_fully_cached(cache_fs, locked)` — bool check (source + all artifacts)
- `artifact::required_artifacts_for_target(locked, ext_manifest, target)` — from Step 3

These are the correct building blocks, but no single function composes them into "ready for frozen build on target X?"

---

## 3. Design

### 3.1 New types

In `lang/crates/vo-module/src/readiness.rs`:

```rust
/// Result of checking whether a single locked module is ready for a frozen build
/// on a specific target.
#[derive(Debug)]
pub enum ModuleReadiness {
    /// Module is fully ready: source verified, all required target artifacts verified.
    Ready {
        /// The resolved required artifacts for this target (may be empty for pure-source).
        artifacts: Vec<ResolvedArtifact>,
    },
    /// Module is not ready.
    NotReady(ReadinessFailure),
}

#[derive(Debug)]
pub struct ResolvedArtifact {
    pub artifact_id: ArtifactId,
    pub cache_path: PathBuf,
    pub size: u64,
    pub digest: Digest,
}

#[derive(Debug)]
pub enum ReadinessFailure {
    /// Source package is not installed or fails integrity check.
    SourceNotReady(InstalledModuleError),
    /// A required target artifact is missing from the lock.
    ArtifactNotDeclaredInLock {
        module: String,
        version: String,
        artifact_id: DeclaredArtifactId,
    },
    /// A required target artifact is missing from cache or fails integrity check.
    ArtifactNotReady {
        module: String,
        version: String,
        artifact_id: ArtifactId,
        detail: InstalledModuleError,
    },
}
```

### 3.2 Single-module readiness check

```rust
/// Check whether a single locked module is ready for a frozen build on `target`.
///
/// `fs` is rooted at the module cache directory.
/// `ext_manifest` is the parsed vo.ext.toml for this module, or None if absent.
pub fn check_module_readiness<F: FileSystem>(
    fs: &F,
    locked: &LockedModule,
    ext_manifest: Option<&ExtensionManifest>,
    target: &str,
) -> ModuleReadiness
```

This function:
1. Computes `module_dir` from `cache::layout::relative_module_dir`.
2. Calls `validate_installed_module(fs, module_dir, locked)`. If it fails → `NotReady(SourceNotReady)`.
3. Calls `artifact::required_artifacts_for_target(locked, ext_manifest, target)`. If it fails → `NotReady(ArtifactNotDeclaredInLock)`.
4. For each required artifact, calls `validate_installed_artifact(fs, path, locked, artifact)`. If any fails → `NotReady(ArtifactNotReady)`.
5. Returns `Ready { artifacts }`.

### 3.3 Project-level readiness check

```rust
/// Check whether all locked dependencies are ready for a frozen build on `target`.
///
/// Returns the first failure encountered, or Ok with all resolved artifacts.
pub fn check_project_readiness<F: FileSystem>(
    fs: &F,
    locked_modules: &[LockedModule],
    ext_manifests: &[(ModulePath, ExtensionManifest)],
    target: &str,
) -> Result<Vec<(ModulePath, Vec<ResolvedArtifact>)>, ReadinessFailure>
```

This function:
1. For each `LockedModule`, finds the matching `ExtensionManifest` (if any) from the provided list.
2. Calls `check_module_readiness` for each.
3. Collects all `Ready` results or returns the first `NotReady`.

### 3.4 Extension manifest discovery helper

To support the project-level check, add a helper that reads extension manifests from installed module cache directories:

```rust
pub fn discover_cached_ext_manifests<F: FileSystem>(
    fs: &F,
    locked_modules: &[LockedModule],
) -> Vec<(ModulePath, ExtensionManifest)>
```

This reads `vo.ext.toml` from each locked module's cache directory and parses it. Modules without `vo.ext.toml` are skipped.

---

## 4. How Consumers Change

### 4.1 `vo-engine/src/compile/pipeline.rs`

**Before** (current):
```rust
// Phase 1: source validation
validate_locked_modules_installed(locked_modules, &mod_cache)?;

// ... analysis happens ...

// Phase 2: native artifact resolution (re-validates source too!)
let extensions = prepare_native_extension_specs_for_frozen_build(
    &self.project.extensions, &self.locked_modules, &self.mod_cache,
)?;
```

**After**:
```rust
// Single readiness check before analysis
let readiness = vo_module::readiness::check_project_readiness(
    &RealFs::new(&mod_cache),
    locked_modules,
    &ext_manifests,
    native::current_target_triple(),
)?;

// ... analysis happens ...

// Convert readiness artifacts to NativeExtensionSpec (runtime concern only)
let extensions = readiness.into_iter()
    .filter_map(|(module, artifacts)| /* build NativeExtensionSpec from artifacts */)
    .collect();
```

**Deletable from `native.rs`**:
- `validate_locked_modules_installed` (entire function)
- `validate_locked_module_installed`
- `validate_installed_module_at_dir`
- `validate_locked_native_artifact_bytes`
- `prepare_native_extension_specs_for_frozen_build` body (keep a thin wrapper that calls readiness API + constructs `NativeExtensionSpec`)

### 4.2 `vo-web/src/module_install/mod.rs`

**Before** (current):
```rust
if validate_vfs_installed_module(&locked).is_ok() {
    load_locked_ext_from_vfs(&locked).await?;
} else {
    install_locked_module_to_vfs(&locked).await?;
}
```

**After** (Step 4 provides the readiness check; Step 5 wires it):
```rust
let readiness = vo_module::readiness::check_module_readiness(
    &vfs, &locked, ext_manifest.as_ref(), "wasm32-unknown-unknown",
);
match readiness {
    ModuleReadiness::Ready { artifacts } => {
        load_wasm_extension_from_artifacts(&locked, &artifacts).await?;
    }
    ModuleReadiness::NotReady(_) => {
        install_locked_module_to_vfs(&locked).await?;
    }
}
```

### 4.3 Studio / `vo-app-runtime`

These consumers never call readiness directly. They go through `vo-engine` (native) or `vo-web` (wasm), both of which now delegate to `vo-module::readiness`. No changes needed.

---

## 5. Test Cases

### 5.1 Module with source ready and no artifacts — Ready

```rust
#[test]
fn readiness_succeeds_for_pure_source_module() {
    // LockedModule with no artifacts, no ext_manifest
    // Cache has valid source markers
    // check_module_readiness -> Ready { artifacts: [] }
}
```

### 5.2 Module with source ready and native artifact ready — Ready

```rust
#[test]
fn readiness_succeeds_when_native_artifact_is_present_and_valid() {
    // LockedModule with extension-native artifact for aarch64-apple-darwin
    // ext_manifest declares aarch64-apple-darwin
    // Cache has valid source + artifact file with matching digest
    // check_module_readiness("aarch64-apple-darwin") -> Ready { artifacts: [1 entry] }
}
```

### 5.3 Source not installed — NotReady(SourceNotReady)

```rust
#[test]
fn readiness_fails_when_source_not_installed() {
    // Empty cache
    // check_module_readiness -> NotReady(SourceNotReady)
}
```

### 5.4 Source ready but artifact missing from cache — NotReady(ArtifactNotReady)

```rust
#[test]
fn readiness_fails_when_artifact_missing_from_cache() {
    // Source installed and valid
    // ext_manifest declares native target
    // Artifact file not in cache
    // check_module_readiness -> NotReady(ArtifactNotReady)
}
```

### 5.5 Source ready but artifact digest mismatch — NotReady(ArtifactNotReady)

```rust
#[test]
fn readiness_fails_when_artifact_digest_mismatches() {
    // Source installed, artifact file exists but wrong content
    // check_module_readiness -> NotReady(ArtifactNotReady)
}
```

### 5.6 Declared target not in lock — NotReady(ArtifactNotDeclaredInLock)

```rust
#[test]
fn readiness_fails_when_declared_target_missing_from_lock() {
    // ext_manifest declares aarch64-apple-darwin
    // locked.artifacts is empty (lock was generated before this target was added)
    // check_module_readiness -> NotReady(ArtifactNotDeclaredInLock)
}
```

### 5.7 Unsupported target — Ready (empty artifacts)

```rust
#[test]
fn readiness_succeeds_for_unsupported_target_with_no_requirement() {
    // ext_manifest declares only aarch64-apple-darwin
    // check_module_readiness("x86_64-unknown-linux-gnu") -> Ready { artifacts: [] }
    // (target simply not declared = not supported = no artifact requirement)
}
```

Wait — this needs careful spec interpretation. §8.4 says:

> If the active build target requires a published dependency's target-specific binary artifact and that dependency version does not declare support for the active target, the build MUST fail.

So if a module has native extensions and the host target is not declared, the build should **fail**, not silently succeed with empty artifacts. The readiness API must distinguish:

- Module has no extension at all → Ready (pure source)
- Module has extension, target is declared → check artifacts
- Module has native extension, but host target is **not** declared → **NotReady** (unsupported target)
- Module has only wasm extension, querying a native target → Ready (no native artifacts needed)

### 5.7 (revised) Native extension without host target support — NotReady

```rust
#[test]
fn readiness_fails_when_native_extension_does_not_support_host_target() {
    // ext_manifest has [extension.native] with only aarch64-apple-darwin
    // check_module_readiness("x86_64-unknown-linux-gnu")
    // -> NotReady(UnsupportedTarget)
}
```

### 5.8 Wasm-only extension queried for native target — Ready (empty)

```rust
#[test]
fn readiness_succeeds_for_wasm_only_module_on_native_target() {
    // ext_manifest has only [extension.wasm], no [extension.native]
    // check_module_readiness("aarch64-apple-darwin") -> Ready { artifacts: [] }
    // (no native extension = no native artifact requirement)
}
```

### 5.9 Project-level: mixed modules

```rust
#[test]
fn project_readiness_checks_all_locked_modules() {
    // 2 locked modules: one pure-source, one with native extension
    // Both installed and valid
    // check_project_readiness -> Ok with correct artifact lists
}
```

### 5.10 Project-level: first failure stops

```rust
#[test]
fn project_readiness_reports_first_failure() {
    // 2 locked modules: first is fine, second is missing artifact
    // check_project_readiness -> Err(ArtifactNotReady for second module)
}
```

---

## 6. Implementation Order

### Phase A — Readiness types and single-module check

1. Create `lang/crates/vo-module/src/readiness.rs`
2. Define `ModuleReadiness`, `ReadinessFailure`, `ResolvedArtifact`
3. Implement `check_module_readiness`
4. Handle the unsupported-native-target case per §8.4
5. Export from `vo-module/src/lib.rs`
6. Unit tests for all single-module scenarios

### Phase B — Project-level readiness and manifest discovery

7. Implement `discover_cached_ext_manifests`
8. Implement `check_project_readiness`
9. Unit tests for project-level scenarios

### Phase C — `vo-engine` migration

10. Rewrite `PreparedProject::load_prepared` to call `check_project_readiness` instead of `validate_locked_modules_installed`
11. Rewrite `AnalyzedCompilation::into_output` to use readiness artifacts instead of `prepare_native_extension_specs_for_frozen_build`
12. Delete superseded validation functions from `native.rs`
13. Keep `ensure_local_native_extension_built` and `build_native_extension` (local workspace concern)
14. `cargo test -p vo-engine --release`

### Phase D — Validation

15. `cargo test -p vo-module --release`
16. `cargo test -p vo-engine --release`
17. `cargo check -p vo --release`
18. `cargo check -p vo-web` (Step 4 does not change vo-web code, but confirm)

---

## 7. Risk Assessment

| Risk | Mitigation |
|---|---|
| §8.4 unsupported-target semantics are subtle | Step 3.7 test + explicit match arm in `check_module_readiness`. Wasm-only modules don't trigger native-unsupported failure. |
| `vo-engine` compile pipeline currently validates source in phase 1 and artifacts in phase 2 (after analysis). Merging into one pre-analysis check changes timing. | Source validation is already pre-analysis. Artifact validation currently happens post-analysis only because it is tangled with `NativeExtensionSpec` construction. Separating validation from construction lets us validate earlier without changing semantics. |
| `check_project_readiness` needs extension manifests, but the engine currently discovers them during analysis (from VFS packages). | Two options: (a) `discover_cached_ext_manifests` reads them from cache before analysis, or (b) split readiness into source-readiness (pre-analysis) + artifact-readiness (post-analysis, once manifests are known). Option (a) is cleaner and matches the spec model where `vo.ext.toml` is part of the published source, not a runtime discovery. |
| `vo-web` migration is deferred to Step 5 | Expected — Step 4 provides the API, Step 5 wires `vo-web`. |

---

## 8. Relationship to Other Steps

- **Step 1** (landed): Provides canonical `ExtensionManifest` types.
- **Step 2** (landed): Publication-time artifact contract. Step 4 is the build-time readiness contract — opposite end of the lifecycle.
- **Step 3** (prerequisite): Provides `required_artifacts_for_target`, which Step 4 calls internally.
- **Step 5** (depends on Steps 3+4): Migrates `vo-web` to call readiness API.
- **Step 6** (depends on Steps 2+4): Integrity cross-validation uses readiness results.

---

## 9. Validation

```sh
cargo test -p vo-module --release
cargo test -p vo-engine --release
cargo check -p vo --release
cargo check -p vo-web
```

---

## 10. Deliverables

- `lang/crates/vo-module/src/readiness.rs` — `check_module_readiness`, `check_project_readiness`, `discover_cached_ext_manifests`
- `ModuleReadiness`, `ReadinessFailure`, `ResolvedArtifact` types
- `vo-engine/src/compile/native.rs` — simplified to thin wrapper over readiness API + `NativeExtensionSpec` construction
- `vo-engine/src/compile/pipeline.rs` — single readiness call replaces two-phase validation
- Comprehensive unit tests in `readiness.rs`
- Existing `vo-engine` tests pass unchanged
