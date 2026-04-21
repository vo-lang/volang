# Step 3: Target-Artifact Resolution API in `vo-module`

**Date**: 2026-04-16
**Status**: Planning
**Parent**: `2026-04-15-vo-module-final-form-master-plan.md` — Step 3
**Depends on**: Step 1 (canonical `vo.ext.toml` schema — landed)
**Normative reference**: `lang/docs/spec/module.md` §8.4

---

## 1. Objective

Move the "which locked artifacts are required for a given target?" logic from `vo-engine/src/compile/native.rs` into `vo-module`, creating a single authoritative API that both `vo-engine` (native) and `vo-web` (wasm) call for frozen-build artifact resolution.

After this step:
- `vo-module` answers: given a `LockedModule` + its `ExtensionManifest` + a host target triple, what are the required `LockedArtifact` entries and their expected cache paths?
- `vo-engine` deletes its duplicated artifact-name derivation and lock-lookup logic.
- `vo-web` can query the same API with `wasm32-unknown-unknown` instead of rolling its own wasm-artifact lookup.

---

## 2. Current State

### 2.1 `vo-engine/src/compile/native.rs` duplicated logic

This file currently contains ~200 lines of module-protocol logic that does not belong in a compiler:

| Function | What it does | Should be in `vo-module` |
|---|---|---|
| `native_extension_artifact_name` | Reads `ExtensionManifest.declared_native_target(host)` to derive the locked library name | Yes — artifact identity derivation |
| `find_locked_native_artifact` | Searches `LockedModule.artifacts` for `(extension-native, host_triple, library_name)` | Yes — locked artifact lookup |
| `locked_native_artifact_path` | Returns `module_dir/artifacts/{name}` | Yes — cache layout |
| `validate_locked_native_artifact_bytes` | Calls `cache::validate::validate_installed_artifact` | Yes — integrity check |
| `locked_module_for_cached_extension` | Infers `(module_path, version)` from cache directory and finds the matching `LockedModule` | Yes — cache identity resolution |
| `module_identity_from_cache_dir` | Parses `github.com@owner@repo/vX.Y.Z` from path | Yes — cache layout knowledge |

The only things that genuinely belong in `vo-engine` are:
- `current_target_triple()` — build-time `env!("VO_TARGET_TRIPLE")`
- `ensure_local_native_extension_built` / `build_native_extension` — local workspace cargo build
- `native_extension_spec` — constructing the runtime `NativeExtensionSpec`

### 2.2 `vo-web` duplicated logic

`vo-web/src/module_install/extension.rs` has its own wasm-artifact resolution:
- `read_wasm_extension_from_vfs` reads `vo.ext.toml` from VFS and returns `WasmExtensionManifest`
- `fetch_and_cache_extension_assets` constructs artifact names and fetches wasm + js_glue
- No call to `declared_artifact_ids` or any lock-level artifact verification

### 2.3 Types already available from Step 1

- `ExtensionManifest` with `declared_artifact_ids() -> Vec<DeclaredArtifactId>`
- `DeclaredArtifactId { kind, target, name }`
- `NativeTargetDeclaration { target, library }`
- `WasmExtensionManifest { kind, wasm, js_glue }`
- `LockedArtifact { id: ArtifactId, size, digest }`

---

## 3. Design

### 3.1 New public API in `vo-module`

Create `lang/crates/vo-module/src/artifact.rs`:

```rust
/// A required artifact resolved from a locked module's extension manifest.
pub struct RequiredArtifact<'a> {
    pub locked_artifact: &'a LockedArtifact,
    pub cache_relative_path: PathBuf,
}

/// Given a locked module, its parsed extension manifest, and a target triple,
/// return the required locked artifacts for that target.
///
/// Returns `Ok(vec![])` if the module has no extension manifest or the target
/// is not declared. Returns `Err` if the manifest declares the target but the
/// lock does not contain the expected artifact.
pub fn required_artifacts_for_target<'a>(
    locked: &'a LockedModule,
    ext_manifest: Option<&ExtensionManifest>,
    target: &str,
) -> Result<Vec<RequiredArtifact<'a>>, Error>
```

This function:
1. If `ext_manifest` is `None`, returns empty — pure-source module.
2. Collects declared `DeclaredArtifactId` entries matching `target` from the manifest.
3. For each declared artifact, finds the matching `LockedArtifact` in `locked.artifacts` by `(kind, target, name)`.
4. If a declared artifact is missing from the lock, returns `Error::MissingArtifact`.
5. Computes `cache_relative_path` as `artifacts/{name}` (existing cache layout convention).

### 3.2 Companion: module identity from cache path

Move `module_identity_from_cache_dir` into `vo-module/src/cache/layout.rs`:

```rust
pub fn module_identity_from_cache_dir(
    mod_root: &Path,
    module_dir: &Path,
) -> Option<(String, String)>
```

This is pure cache-layout knowledge that the engine should not own.

### 3.3 Companion: locked module lookup

Add to `vo-module/src/artifact.rs` (or `schema/lockfile.rs`):

```rust
pub fn find_locked_module_for_cache_dir<'a>(
    mod_root: &Path,
    module_dir: &Path,
    locked_modules: &'a [LockedModule],
) -> Option<&'a LockedModule>
```

### 3.4 How `vo-engine` changes

After this step, `prepare_cached_extension_spec` in `native.rs` becomes:

```rust
fn prepare_cached_extension_spec(
    manifest: &ExtensionManifest,
    locked: &LockedModule,
    module_dir: &Path,
) -> Result<NativeExtensionSpec, ModuleSystemError> {
    let required = vo_module::artifact::required_artifacts_for_target(
        locked, Some(manifest), current_target_triple(),
    ).map_err(/* ... */)?;

    // Validate each required artifact's bytes
    for artifact in &required {
        validate_installed_artifact_at_dir(module_dir, locked, artifact.locked_artifact)?;
    }

    // The native extension path is the first extension-native artifact
    let native_artifact = required.iter()
        .find(|a| a.locked_artifact.id.kind == "extension-native")
        .expect("required_artifacts_for_target returned native artifacts");
    Ok(native_extension_spec(manifest, module_dir.join(&native_artifact.cache_relative_path)))
}
```

**Deletable functions** from `vo-engine/src/compile/native.rs`:
- `native_extension_artifact_name`
- `find_locked_native_artifact`
- `locked_native_artifact_path`
- `module_identity_from_cache_dir`
- `locked_module_for_cached_extension` (replaced by `find_locked_module_for_cache_dir`)

### 3.5 How `vo-web` benefits

`vo-web` can call:
```rust
let required = vo_module::artifact::required_artifacts_for_target(
    locked, Some(&ext_manifest), "wasm32-unknown-unknown",
)?;
```

This tells it exactly which wasm + js_glue artifacts to fetch, with locked sizes and digests for verification. Step 5 will do the full migration; Step 3 only provides the API.

---

## 4. Test Cases

### 4.1 Native target with matching locked artifact — resolves

```rust
#[test]
fn required_artifacts_resolves_native_target() {
    // ext_manifest declares aarch64-apple-darwin with library libdemo.dylib
    // locked.artifacts includes extension-native:aarch64-apple-darwin:libdemo.dylib
    // required_artifacts_for_target("aarch64-apple-darwin") returns 1 entry
}
```

### 4.2 WASM target with wasm + js_glue — resolves both

```rust
#[test]
fn required_artifacts_resolves_wasm_bindgen_target() {
    // ext_manifest declares bindgen wasm with js_glue
    // locked.artifacts includes extension-wasm + extension-js-glue
    // required_artifacts_for_target("wasm32-unknown-unknown") returns 2 entries
}
```

### 4.3 Declared target not in lock — error

```rust
#[test]
fn required_artifacts_fails_when_lock_missing_declared_artifact() {
    // ext_manifest declares aarch64-apple-darwin
    // locked.artifacts is empty
    // required_artifacts_for_target("aarch64-apple-darwin") returns MissingArtifact
}
```

### 4.4 Unsupported target — empty result

```rust
#[test]
fn required_artifacts_returns_empty_for_unsupported_target() {
    // ext_manifest declares only aarch64-apple-darwin
    // required_artifacts_for_target("x86_64-unknown-linux-gnu") returns Ok(vec![])
}
```

### 4.5 No extension manifest — empty result

```rust
#[test]
fn required_artifacts_returns_empty_without_ext_manifest() {
    // ext_manifest is None
    // required_artifacts_for_target("aarch64-apple-darwin") returns Ok(vec![])
}
```

### 4.6 Cache path derivation

```rust
#[test]
fn required_artifact_cache_path_is_correct() {
    // Verify cache_relative_path == "artifacts/{artifact_name}"
}
```

### 4.7 `module_identity_from_cache_dir` round-trip

```rust
#[test]
fn module_identity_from_cache_dir_parses_canonical_layout() {
    // cache_dir("github.com/acme/lib", "v1.2.3") -> relative_module_dir
    // module_identity_from_cache_dir(mod_root, full_dir) -> ("github.com/acme/lib", "v1.2.3")
}
```

---

## 5. Implementation Order

### Phase A — `vo-module` API

1. Create `lang/crates/vo-module/src/artifact.rs`
2. Implement `required_artifacts_for_target`
3. Move `module_identity_from_cache_dir` into `cache/layout.rs`
4. Add `find_locked_module_for_cache_dir`
5. Export from `vo-module/src/lib.rs`
6. Add unit tests in `artifact.rs`

### Phase B — `vo-engine` migration

7. Rewrite `prepare_cached_extension_spec` to call `required_artifacts_for_target`
8. Delete `native_extension_artifact_name`, `find_locked_native_artifact`, `locked_native_artifact_path`, `locked_module_for_cached_extension`, `module_identity_from_cache_dir` from `native.rs`
9. Update `native.rs` tests if any directly test the deleted functions
10. Verify `cargo test -p vo-engine --release`

### Phase C — Validation

11. `cargo test -p vo-module --release`
12. `cargo test -p vo-engine --release`
13. `cargo check -p vo --release`
14. `cargo check -p vo-web` (Step 3 does not change vo-web, but confirm it still compiles)

---

## 6. Risk Assessment

| Risk | Mitigation |
|---|---|
| `vo-engine` behavior subtly changes when switching from inline artifact lookup to `vo-module` API | The logic is identical — we are lifting, not rewriting. Tests remain the same. |
| `required_artifacts_for_target` returns wrong set for edge cases (e.g., bindgen wasm with js_glue) | Step 2 tests already exercise `declared_artifact_ids` for all combinations. Step 3 tests extend that to lock-level matching. |
| Cache layout assumption (`artifacts/{name}`) is fragile | This is already the sole convention used everywhere. Centralizing it makes future changes easier, not harder. |
| `vo-web` still has its own artifact logic after Step 3 | Expected — Step 5 will migrate `vo-web`. Step 3 only provides the API. |

---

## 7. Relationship to Other Steps

- **Step 1** (landed): Provides `ExtensionManifest`, `DeclaredArtifactId`, `declared_artifact_ids()`.
- **Step 2** (landed): Validates declared vs staged artifacts at publication time. Step 3 validates declared vs locked artifacts at build time.
- **Step 4** (parallel): Step 4 builds the unified readiness API. It will call `required_artifacts_for_target` as a building block.
- **Step 5** (depends on Step 3): Migrates `vo-web` to use the same API.

---

## 8. Validation

```sh
cargo test -p vo-module --release
cargo test -p vo-engine --release
cargo check -p vo --release
cargo check -p vo-web
```

---

## 9. Deliverables

- `lang/crates/vo-module/src/artifact.rs` — `required_artifacts_for_target`, `find_locked_module_for_cache_dir`
- `lang/crates/vo-module/src/cache/layout.rs` — `module_identity_from_cache_dir` (moved from engine)
- `lang/crates/vo-engine/src/compile/native.rs` — simplified to call `vo-module` API, ~100 lines deleted
- Unit tests in `artifact.rs` covering all target/manifest/lock combinations
- Existing `vo-engine` tests pass unchanged
