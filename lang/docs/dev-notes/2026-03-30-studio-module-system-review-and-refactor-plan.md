# Studio & Module System Review — Problems and Refactor Plan

**Date:** 2026-03-30
**Triggered by:** Persistent `vo.lock does not pin an extension-native artifact` error in Studio native when running `chat_demo` with `vogui` dependency.

**Status (updated after native refactor landing):**
- Phases 1-7 below have landed on the native / web project-preparation path.
- Native `vo-engine`, `vo-web`, and `studio/wasm` now share one authoritative `vo-module::project::ProjectDeps` contract for project `vo.mod` / `vo.lock` reading and lock validation.
- Compile-cache invalidation is fixed for compiler identity, but local native rebuild state is still not fingerprinted.

---

## 1. Context

Running `chat_demo/main.vo` (which imports `vogui`) from Studio native produces:

```
Analysis error: vo.lock does not pin an extension-native artifact
for github.com/vo-lang/vogui@v0.1.2 (libvo_vogui.dylib)
```

Multiple targeted patches were applied but the error persisted, prompting a full-system review of the Studio compilation pipeline and the module system's native extension handling.

---

## 2. Call-Chain Map

### 2.1 Native Studio

Studio native now shares a common auto-install root, but `cmd_check_vo` exits through the lightweight engine check path instead of the full compile path:

```
cmd_check_vo   ─────────────▶ check_with_auto_install(path)
                                   │
                                   ├── auto_download_locked_modules(root)
                                   └── check(path)
                                         └── check_with_fs()
                                               ├── analyze_project()
                                               └── prepare_extension_manifests_for_frozen_build()

cmd_compile_vo ─┐
cmd_build_vo   ─┼──▶ prepare_and_compile()
cmd_dump_vo    ─┤         │
cmd_run_gui    ─┘         ▼
                   compile_with_auto_install(path)
                         │
                         ├── auto_download_locked_modules(root)
                         │       ├── download_all()  (vo-module materialize)
                         │       └── prepare_extension_manifests()  ◀── shared native-extension build path
                         │
                         └── compile_with_cache(path)
                                 └── compile(path)
                                         └── compile_with_fs()
                                                 ├── analyze_project()
                                                 ├── prepare_extension_manifests_for_frozen_build()
                                                 └── resolve_extension_manifests()
```

Key observation: Studio native still shares one authoritative auto-install root, but `cmd_check_vo` no longer pays the codegen / resolve cost of `prepare_and_compile()`.

### 2.2 Web Studio (WASM)

```
WebBackend.runGui()
    │
    ├── wasm.prepareEntry(path)
    │       │
    │       ├── project mode: vo_web::ensure_vfs_deps_from_fs()
    │       └── single-file mode: vo_web::resolve_and_install_module() per external module
    │
    └── wasm.runGui(path)  /  wasm.compileRunEntry(path)
            │
            └── compile_from_vfs()
                    └── vo_web::compile_entry_with_vfs()
```

Web Studio's `checkVo` / `compileVo` / `buildVo` / `dumpVo` all `throw new Error('not wired')`.

### 2.3 CLI (`vo` binary)

```
vo run   → compile_with_auto_install()
vo build → compile_with_auto_install()
vo check → check_with_auto_install()
vo test  → compile_with_auto_install()
vo emit  → compile_with_auto_install()
```

User-facing CLI commands now align with Studio native on dependency auto-install semantics. The lower-level engine `compile()` / `check()` APIs still exist as frozen / pre-prepared primitives.

---

## 3. Verified Problems

Each problem below was verified against the source code during the review. Some entries are now historical and are marked as fixed or partially fixed so the original reasoning is preserved without pretending the current code is still in the old state.

### P1. Studio `check` runs full compile (fixed)

**Files:** `studio/src-tauri/src/commands/compiler.rs`, `vo-engine/src/compile.rs`

`cmd_check_vo` now calls `check_with_auto_install()`, which keeps the shared dependency-install path but stops after analysis + frozen extension preparation.

**Status:** Fixed on the native Studio path.

### P2. Native extension policy is implemented three times with divergent behavior

Three separate code paths handle native extensions for cached modules:

| # | Location | Function | Called From |
|---|----------|----------|-------------|
| 1 | compile.rs:388-399 | `ensure_extension_manifests_built` | `auto_download_locked_modules` (download phase) |
| 2 | compile.rs:1380/1419 | `validate_extension_manifests_for_frozen_build` | `compile_with_fs` / `check_with_fs` (validation phase) |
| 3 | compile.rs:1425 | `resolve_extension_manifests` | `compile_with_fs` (resolve phase) |

Path #1 and #2 have nearly identical logic:
```
for each manifest:
    if module_dir starts_with mod_root:
        locked = locked_module_for_cached_extension()
        validate_installed_module()
        validate_locked_native_artifact()   ← has local-build fallback
    else:
        ensure_local_native_extension_built()
```

But path #3 (`resolve_extension_manifest`, line 1791-1814) does **not** have the local-build fallback:
```rust
fn resolve_extension_manifest(...) -> Result<ExtensionManifest, String> {
    // ...
    if !module_dir.starts_with(mod_root) {
        return Ok(manifest.clone());  // local override: return as-is
    }
    let locked = locked_module_for_cached_extension(...)?;
    let artifact = find_locked_native_artifact(manifest, locked)?;  // ← NO FALLBACK
    // ...
}
```

**This is the direct cause of the persistent error.** Even after `validate_locked_native_artifact` was patched to fall back to local build, `resolve_extension_manifest` still calls `find_locked_native_artifact` without fallback, and that function returns the raw error:

```rust
fn find_locked_native_artifact(...) -> Result<&LockedArtifact, String> {
    // ...
    .ok_or_else(|| format!(
        "vo.lock does not pin an extension-native artifact for {}@{} ({})",
        locked.path, locked.version, artifact_name,
    ))
}
```

**Impact:**
- The `compile_with_auto_install` path first runs `auto_download_locked_modules` (which succeeds via fallback), then runs `compile_with_fs` → `validate_extension_manifests_for_frozen_build` (which succeeds via fallback), then reaches `resolve_extension_manifests` (which **fails** because it has no fallback).
- Fixing validate without fixing resolve means the error message is unchanged.

### P3. `validate_*` functions have mutation side effects

`validate_extension_manifests_for_frozen_build` (line 1719-1755) calls `ensure_local_native_extension_built` for local-override modules, which triggers `cargo build`. The function name strongly implies a pure validation, but it writes to disk.

Similarly, `ensure_extension_manifests_built` (line 1948-1984) is semantically a build step but is called as part of "download".

**Impact:**
- Hard to reason about what's a read-only check vs. a write operation.
- Test isolation is fragile; calling validate may mutate the filesystem.

### P4. Web and native Studio use completely separate dependency preparation systems

 | Aspect | Native Studio | Web Studio |
 |--------|---------------|------------|
 | Dependency install | `vo_engine::auto_download_locked_modules` | `vo_web::ensure_vfs_deps_from_fs` / `vo_web::resolve_and_install_module` |
 | Compilation | `vo_engine::compile_with_fs` (RealFs) | `vo_web::compile_entry_with_vfs` (MemoryFs) |
 | Extension loading | Native dylib via `resolve_extension_manifests` | WASM artifact via VFS preload |
 | Module cache | `~/.vo/mod/` on real filesystem | Browser VFS (IndexedDB-backed) |
 | Lock semantics | `vo.lock` required, frozen-build validation | Synthetic `vo.lock` generated from registry |
 | Check/Compile/Build | Via Tauri commands → vo-engine | `throw new Error('not wired')` |

**Impact:**
- Bugs fixed in one backend don't propagate to the other.
- Module-system semantics (what's valid, what's required) can diverge silently.
- No shared "project preparation" abstraction exists.

There was also a **Studio vs CLI** semantic split on native. That gap is now substantially reduced: Studio native and the user-facing CLI commands (`run` / `build` / `check` / `test` / `emit`) all use the engine auto-install path. The remaining distinction is intentional and lives at the low-level engine API boundary (`compile()` / `check()` vs `compile_with_auto_install()` / `check_with_auto_install()`).

### P5. Compile cache fingerprint does not fully capture compiler / extension state (partially fixed)

**Files:** `vo-engine/src/compile.rs`, `vo-engine/build.rs`

The fingerprint hashes:
- ✅ Schema version (manual string `"3"`)
- ✅ Target triple
- ✅ Source root path
- ✅ Mod cache root path
- ✅ Source tree file contents (`.vo`, `vo.mod`, `vo.lock`, `vo.ext.toml`, `vo.work`)
- ✅ Workspace replace roots and their file trees
- ✅ Compiler package version
- ✅ Compiler build ID generated by `vo-engine/build.rs`
- ❌ **Module cache contents** (cached source files, built native libraries)
- ❌ **Native extension build state** (presence/mtime of `.dylib`)

The cache is no longer blind to compiler rebuilds, but it is still blind to local native rebuild state inside the module cache.

**Status:** Fixed for compiler identity. Remaining gap: local native artifact state is still not fingerprinted.

### P6. Studio diagnostics flatten all errors to unstructured strings (fixed on native)

**Files:** `vo-engine/src/compile.rs`, `studio/src-tauri/src/commands/compiler.rs`, `studio/src/lib/types.ts`

Native Studio diagnostics now preserve `CompileError` category plus module-system metadata (`moduleStage`, `moduleKind`, `modulePath`, `moduleVersion`) when the backend reports `CompileError::ModuleSystem(ModuleSystemError)`.

**Status:** Fixed on the native Studio path. Remaining gap: the web backend still does not implement `checkVo` / `compileVo` / `buildVo`.

### P7. Release packaging included monorepo `[patch]` sections in source packages (fixed)

**File:** `vo-release/src/repo.rs:320-393`

`build_source_package` now strips `[patch.*]` sections from `Cargo.toml` files before packaging. Before this fix, cached modules contained `Cargo.toml` files with `[patch."https://github.com/vo-lang/volang"]` entries pointing to `../../volang/lang/crates/...` — paths that don't exist in the module cache.

**Status:** Fixed in previous session. Unit tests added.

---

## 4. Root Cause Analysis

The persistent `vo.lock does not pin an extension-native artifact` error has **two root causes**:

### Root Cause A: `resolve_extension_manifest` lacks local-build fallback

The `validate_locked_native_artifact` function was correctly patched to fall back to `ensure_local_native_extension_built` when no locked artifact exists. But `resolve_extension_manifest` — called immediately after validation in `compile_with_fs` — still calls `find_locked_native_artifact` without any fallback. Since native extensions are always built locally (never uploaded as release artifacts), this function always fails for modules without pre-built native artifacts in `vo.lock`.

### Root Cause B: No unified native extension contract

The module system has no explicit, documented rule for how native extensions work:
- Are native artifacts registry artifacts? (No — they're always built locally)
- Does `vo.lock` need to pin native artifacts? (Currently required by `find_locked_native_artifact`, but this contradicts the "always local build" reality)
- Should validation require or just prefer locked artifacts? (Different code paths answer differently)

---

## 5. Refactor Plan

### Phase 1: Fix the immediate bug (P2 resolve fallback)

**Goal:** Make `resolve_extension_manifest` consistent with `validate_locked_native_artifact`.

When `find_locked_native_artifact` fails for a cached module, `resolve_extension_manifest` should fall back to the locally-built native path (the same path `ensure_local_native_extension_built` would produce).

Concretely, in `resolve_extension_manifest`:
```rust
// Before (current):
let artifact = find_locked_native_artifact(manifest, locked)?;
let mut resolved = manifest.clone();
resolved.native_path = locked_native_artifact_path(&module_dir, artifact);

// After:
let mut resolved = manifest.clone();
match find_locked_native_artifact(manifest, locked) {
    Ok(artifact) => {
        resolved.native_path = locked_native_artifact_path(&module_dir, artifact);
    }
    Err(_) => {
        // Native extensions are built locally; use the local build output path.
        // The validate phase already ensured the build succeeded.
    }
};
```

**Files:** `vo-engine/src/compile.rs` (resolve_extension_manifest)
**Tests:** Extend `test_validate_locked_extension_manifests_falls_back_to_local_build_without_locked_artifact` to also verify that `resolve_extension_manifests` succeeds.

### Phase 2: Consolidate extension policy into a single entry point

**Status:** Landed in a lighter-weight form. The public wrappers still exist, but cached-module native-extension preparation now funnels through shared per-manifest helpers instead of duplicated policy.

**Goal:** Replace the three separate extension-handling code paths with one.

Create a single `ExtensionPreparer` (or equivalent function) that owns the full lifecycle:

```
prepare_extensions(manifests, locked_modules, mod_root, mode) -> Result<Vec<ExtensionManifest>>
    where mode = Validate | Build | Resolve

    for each manifest:
        classify(manifest, mod_root):
            LocalOverride  → ensure_local_native_extension_built()
            CachedModule   → validate_installed_module()
                             build_if_needed()
                             resolve_native_path()
```

Remove:
- `validate_extension_manifests_for_frozen_build` (merged into prepare)
- `ensure_extension_manifests_built` (merged into prepare)
- `resolve_extension_manifests` (merged into prepare)

**Files:** `vo-engine/src/compile.rs`
**Tests:** Existing tests should pass with the consolidated function.

### Phase 3: Make Studio `check` use the engine's `check()` path

**Status:** Landed. Studio native now calls `check_with_auto_install()`.

**Goal:** Studio's `cmd_check_vo` should not run full compilation.

Option A (minimal): Call `vo_engine::check()` instead of `prepare_and_compile()` from `cmd_check_vo`. This still needs auto-install, so introduce `check_with_auto_install()`:

```rust
pub fn check_with_auto_install(path: &str) -> Result<(), CompileError> {
    let p = Path::new(path);
    let root = source_root(p);
    auto_download_locked_modules(&root)?;
    check(path)
}
```

Option B (better): Introduce a `PrepareMode` enum that controls the pipeline depth:

```rust
pub enum PrepareMode {
    Check,     // analyze + validate extensions, no codegen
    Compile,   // analyze + validate + codegen + resolve extensions
}
```

**Files:** `vo-engine/src/compile.rs`, `studio/src-tauri/src/commands/compiler.rs`

### Phase 4: Add compiler identity to cache fingerprint

**Status:** Landed. The fingerprint now includes the compiler package version plus `VO_COMPILER_BUILD_ID` from the engine build script.

**Goal:** Cache invalidation should be automatic when the compiler changes.

Add to `compute_compile_cache_fingerprint`:
```rust
hasher.update_str("compiler_version", env!("CARGO_PKG_VERSION"));
hasher.update_str("compiler_build_id", env!("VO_BUILD_ID"));  // set in build.rs
```

Where `VO_BUILD_ID` is generated in the vo-engine build script from either:
- `git rev-parse HEAD` (dev builds)
- A release version string (release builds)

**Files:** `vo-engine/src/compile.rs`, `vo-engine/build.rs` (new or extended)

### Phase 5: Rename validate functions to reflect side effects

**Status:** Landed.

**Goal:** Functions that may trigger builds should not be named `validate_*`.

| Historical Name | Landed Outcome |
|---|---|
| `validate_extension_manifests_for_frozen_build` | renamed to `prepare_extension_manifests_for_frozen_build` |
| `ensure_extension_manifests_built` | renamed to `prepare_extension_manifests` |
| `validate_locked_native_artifact` | removed during Phase 2 consolidation |
| `validate_installed_module` | Keep (this one is actually pure validation) |

### Phase 6: Structured error reporting from engine to Studio

**Status:** Landed on the native path.

**Goal:** Module-system errors should carry structured metadata.

Extend `CompileError` with a structured variant:

```rust
pub enum CompileError {
    Io(std::io::Error),
    Parse(String),
    Analysis(String),
    Codegen(String),
    ModuleSystem(ModuleSystemError),  // new
}

pub struct ModuleSystemError {
    pub stage: ModuleSystemStage,
    pub kind: ModuleSystemErrorKind,
    pub module_path: Option<String>,
    pub version: Option<String>,
    pub path: Option<String>,
    pub detail: String,
}

pub enum ModuleSystemErrorKind {
    Missing,
    ReadFailed,
    ParseFailed,
    ValidationFailed,
    DownloadFailed,
    BuildFailed,
    VerificationFailed,
    Mismatch,
    MissingMetadata,
}
```

Studio native `DiagnosticError` now carries `category`, `moduleStage`, `moduleKind`, `modulePath`, and `moduleVersion` in addition to the display message.

**Files:** `vo-engine/src/compile.rs`, `studio/src-tauri/src/commands/compiler.rs`

### Phase 7: Unify web/native project preparation contract

**Status:** Landed for project preparation and lock validation.

**Goal:** Both backends share the same dependency-resolution and lock-validation semantics.

This refactor landed by moving the shared project contract into `vo-module::project` instead of keeping parallel readers in `vo-engine` and `vo-web`.

The shared layer now provides:

```
pub struct ProjectDeps {
    pub has_mod_file: bool,
    pub current_module: Option<String>,
    pub allowed_modules: Vec<String>,
    pub locked_modules: Vec<LockedModule>,
    pub mod_file_path: Option<PathBuf>,
    pub lock_file_path: Option<PathBuf>,
    pub mod_file: Option<ModFile>,
    pub lock_file: Option<LockFile>,
}

pub fn read_project_deps(fs, excluded_modules) -> Result<ProjectDeps, ProjectDepsError>
pub fn read_project_deps_near(fs, dir, excluded_modules) -> Result<ProjectDeps, ProjectDepsError>
```

Current callers:

- `vo-engine` uses the shared readers for `auto_download_locked_modules`, `check_with_fs`, `compile_with_fs`, and workspace-replace filtered dependency planning.
- `vo-web` uses the shared readers for `compile_entry_with_mod_fs`, `compile_entry_with_vfs`, `compile_source_with_vfs`, and VFS dependency preparation.
- `studio/wasm` uses the shared readers when discovering framework modules from project locks.

The intentionally separate part remains the *installation/loading backend*:

- Native still installs into `~/.vo/mod` and builds local dylibs.
- Web still installs into the browser VFS and loads wasm/js glue.

**Files:** `vo-module/src/project.rs`, `vo-engine/src/compile.rs`, `vo-web/src/lib.rs`, `vo-web/src/module_install.rs`, `studio/wasm/src/lib.rs`

---

## 6. Priority and Ordering

| Phase | Priority | Effort | Blocks |
|-------|----------|--------|--------|
| Phase 1 | **Critical** | Small (< 1hr) | Fixes the immediate bug |
| Phase 2 | High | Medium (half day) | Prevents future divergence |
| Phase 3 | High | Small (< 1hr) | Correctness of Studio check |
| Phase 4 | Medium | Small (< 1hr) | Dev experience |
| Phase 5 | Low | Trivial | Code clarity |
| Phase 6 | Medium | Medium (half day) | UX quality |
| Phase 7 | Low | Large (multi-day) | Long-term architecture |

Recommended execution order: **1 → 2 → 3 → 4 → 5 → 6 → 7**

Phase 1 should be done immediately — it is a one-function fix that unblocks `chat_demo`.

---

## 7. Appendix: Code References

| Item | File | Lines |
|------|------|-------|
| Studio cmd_check_vo | `studio/src-tauri/src/commands/compiler.rs` | 116-127 |
| Studio cmd_run_gui | `studio/src-tauri/src/commands/gui.rs` | 98-156 |
| prepare_and_compile | `studio/src-tauri/src/commands/compiler.rs` | 63-65 |
| diagnostic_from_compile_error | `studio/src-tauri/src/commands/compiler.rs` | 67-98 |
| Shared ProjectDeps contract | `vo-module/src/project.rs` | 1-255 |
| Shared ProjectDeps near/root readers | `vo-module/src/project.rs` | 77-235 |
| check_with_auto_install | `vo-engine/src/compile.rs` | 437-441 |
| compile_with_auto_install | `vo-engine/src/compile.rs` | 430-435 |
| auto_download_locked_modules | `vo-engine/src/compile.rs` | 446-549 |
| Native shared project-deps loader | `vo-engine/src/compile.rs` | 1638-1663 |
| prepare_extension_manifests | `vo-engine/src/compile.rs` | 2297-2305 |
| prepare_extension_manifests_for_frozen_build | `vo-engine/src/compile.rs` | 2028-2064 |
| resolve_extension_manifests | `vo-engine/src/compile.rs` | 2070-2082 |
| resolve_extension_manifest | `vo-engine/src/compile.rs` | 2084-2089 |
| find_locked_native_artifact | `vo-engine/src/compile.rs` | 2191-2201 |
| ensure_local_native_extension_built | `vo-engine/src/compile.rs` | 2308-2350 |
| build_native_extension | `vo-engine/src/compile.rs` | 2346-2388 |
| compile_with_fs | `vo-engine/src/compile.rs` | 1570-1608 |
| check_with_fs | `vo-engine/src/compile.rs` | 1532-1564 |
| compute_compile_cache_fingerprint | `vo-engine/src/compile.rs` | 2512-2562 |
| COMPILE_CACHE_SCHEMA_VERSION | `vo-engine/src/compile.rs` | 26 |
| Web shared project-deps compile wiring | `vo-web/src/lib.rs` | 283-293; 443-518 |
| Web shared project-deps install wiring | `vo-web/src/module_install.rs` | 120-149; 618-640; 767-785 |
| Web compile_from_vfs | `studio/wasm/src/lib.rs` | 816-830 |
| Studio wasm shared project-deps loader | `studio/wasm/src/lib.rs` | 584-590; 694-705 |
| Web checkVo (not wired) | `studio/src/lib/backend/web_backend.ts` | 401-403 |
| strip_cargo_patch_sections | `vo-release/src/repo.rs` | 365-393 |
| vo CLI run/build path | `cmd/vo/src/main.rs` | 113-204 |
| vo CLI check path | `cmd/vo/src/main.rs` | 207-218 |
