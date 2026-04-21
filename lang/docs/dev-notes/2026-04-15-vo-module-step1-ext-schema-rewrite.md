# Step 1: Canonical `vo.ext.toml` Schema Rewrite and Legacy Rejection

**Date**: 2026-04-15
**Status**: Planning
**Parent**: `2026-04-15-vo-module-final-form-master-plan.md` — Step 1
**Normative reference**: `lang/docs/spec/native-ffi.md` §4

---

## 1. Objective

Rewrite `vo-module/src/ext_manifest.rs` so that:

1. It parses only the canonical schema defined in `native-ffi.md` §4.3–4.9.
2. Legacy `vo.ext.toml` shapes (`[extension].path` instead of `[extension.native].path`) produce a hard parse error.
3. The typed output exposes the full target-support contract needed by downstream steps (release validation, artifact resolution, readiness checks).

---

## 2. Current State

### 2.1 Current types (`ext_manifest.rs`)

```rust
pub struct ExtensionManifest {
    pub name: String,
    pub native_path: PathBuf,      // resolved from [extension].path — LEGACY
    pub manifest_path: PathBuf,
    pub wasm: Option<WasmExtensionManifest>,
}
```

- `native_path` is derived from `[extension].path` — the legacy flat schema.
- There is no `NativeTargetDeclaration` or `[[extension.native.targets]]` concept.
- `resolve_library_path` uses `#[cfg(target_os)]` to append platform extensions — a host-specific inference, not a declared published artifact name.
- `WasmExtensionManifest` structure is close to canonical but lacks `target = "wasm32-unknown-unknown"` awareness.

### 2.2 Current public API surface

| Function | Used by |
|---|---|
| `discover_extensions(pkg_root)` | `vo-engine` compile pipeline, `vo-engine` toolchain install |
| `extension_name_from_content(content)` | `vo-analysis` VFS |
| `wasm_extension_from_content(content)` | `vo-web` extension loader |
| `include_paths_from_content(content)` | `vo-release` staging, `vo-module` cache install |
| `is_bindgen_ext_content(content)` | `vo-web` |

### 2.3 Consumer call sites (14 total across 10 files)

**`vo-engine`**:
- `compile/mod.rs` — imports `ExtensionManifest`
- `compile/native.rs` — imports `ExtensionManifest`; calls `discover_extensions`
- `compile/tests/cases.rs` — calls `discover_extensions` (3 uses)
- `toolchain.rs` — calls `discover_extensions`

**`vo-analysis`**:
- `project.rs` — imports `discover_extensions`, `ExtensionManifest`
- `vfs.rs` — imports `extension_name_from_content`

**`vo-web`**:
- `module_install/extension.rs` — imports `WasmExtensionKind`, `WasmExtensionManifest`; calls `wasm_extension_from_content`

**`vo-release`**:
- `repo.rs` — calls `include_paths_from_content`

**`vo-module` internal**:
- `cache/install.rs` — calls `include_paths_from_content`

---

## 3. Target Types

The new types must faithfully represent the canonical schema from `native-ffi.md` §4.3.

### 3.1 `ExtensionManifest` (rewritten)

```rust
pub struct ExtensionManifest {
    pub name: String,
    pub include: Vec<PathBuf>,
    pub native: Option<NativeExtensionConfig>,
    pub wasm: Option<WasmExtensionManifest>,
    pub manifest_path: PathBuf,
}
```

### 3.2 `NativeExtensionConfig` (new)

```rust
pub struct NativeExtensionConfig {
    /// Local workspace build hint, may contain `{profile}`.
    pub path: String,
    /// Declared published targets.
    pub targets: Vec<NativeTargetDeclaration>,
}
```

### 3.3 `NativeTargetDeclaration` (new)

```rust
pub struct NativeTargetDeclaration {
    /// Canonical Rust target triple.
    pub target: String,
    /// Published shared-library asset file name.
    pub library: String,
}
```

### 3.4 `WasmExtensionManifest` (adjusted)

```rust
pub struct WasmExtensionManifest {
    pub kind: WasmExtensionKind,
    /// Published WASM binary asset name.
    pub wasm: String,
    /// Published JS glue asset name (bindgen only).
    pub js_glue: Option<String>,
}
```

`WasmExtensionKind` stays as-is: `Standalone` | `Bindgen`.

### 3.5 Convenience methods on `ExtensionManifest`

```rust
impl ExtensionManifest {
    /// Resolve the native library path for the host platform during local development.
    /// Only valid for workspace/local modules, never for published dependencies.
    pub fn resolve_local_native_path(&self, module_root: &Path) -> Option<PathBuf> { ... }

    /// Iterate all declared published artifact identities (native + wasm + js_glue).
    pub fn declared_artifact_ids(&self) -> Vec<DeclaredArtifactId> { ... }

    /// Whether this manifest declares support for a specific target triple.
    pub fn supports_target(&self, target: &str) -> bool { ... }
}
```

---

## 4. Parsing Logic

### 4.1 Legacy rejection (must come first in parse flow)

After reading `[extension]`, immediately check for `path` at the `[extension]` level:

```rust
if extension_table.contains_key("path") {
    return Err(ExtManifestParse(
        "vo.ext.toml uses legacy schema: [extension].path is invalid; \
         use [extension.native].path instead (see spec/native-ffi.md §4.10)"
    ));
}
```

This check fires before any other field extraction.

### 4.2 Parse `[extension]` table

- `name` — required, non-empty string
- `include` — optional array of non-empty relative path strings
- Unknown keys — reserved; reject (except subtables `native`, `wasm`)

### 4.3 Parse `[extension.native]` table (optional)

- `path` — required, non-empty string
- Then parse `[[extension.native.targets]]`:
  - Each entry requires `target` (non-empty) and `library` (non-empty, file name only)
  - Duplicate `target` is an error
  - At least one entry required if `[extension.native]` is present

### 4.4 Parse `[extension.wasm]` table (optional)

Same logic as current `parse_wasm_extension_table`, but tighten:
- `wasm` must be a file name, not a path (reject if contains `/` or `\`)
- `js_glue` must be a file name if present

### 4.5 Top-level validation

- At least one of `[extension.native]` or `[extension.wasm]` must be present (§4.9)
- Cross-check: if neither is present, hard error

### 4.6 Host-specific `resolve_library_path`

Move `resolve_library_path` out of the parse path entirely. It becomes `ExtensionManifest::resolve_local_native_path`, which is only called for workspace/local modules, never during published-dependency processing.

---

## 5. Public API Changes

### 5.1 Functions that stay (with signature changes)

| Function | Change |
|---|---|
| `discover_extensions(pkg_root)` | Returns `Vec<ExtensionManifest>` with new type shape |
| `extension_name_from_content(content)` | No change needed; only reads `[extension].name` |
| `wasm_extension_from_content(content)` | No change needed in return type |
| `include_paths_from_content(content)` | No change needed |
| `is_bindgen_ext_content(content)` | No change needed |

### 5.2 Functions that are new

| Function | Purpose |
|---|---|
| `parse_ext_manifest_content(content, manifest_path)` | Parse from string, used by content-based APIs and tests |

### 5.3 Functions removed

| Function | Reason |
|---|---|
| (internal) `resolve_library_path` at module scope | Moved to `ExtensionManifest::resolve_local_native_path` |

---

## 6. Consumer Migration

### 6.1 `vo-engine/src/compile/native.rs`

Currently accesses `ExtensionManifest.native_path`.

After rewrite:
- For **workspace/local** modules: call `manifest.resolve_local_native_path(module_root)`
- For **frozen-build published** modules: use `manifest.native.targets` to find the declared library name for the host target, then look up the locked artifact in cache

This is the main semantic change at the consumer level.

### 6.2 `vo-engine/src/toolchain.rs`

Currently calls `discover_extensions` then passes manifests to `prepare_native_extension_specs`.
After rewrite: same flow, but `prepare_native_extension_specs` receives the new `ExtensionManifest` type.

### 6.3 `vo-engine/src/compile/tests/cases.rs`

Update test fixture `vo.ext.toml` files from legacy shape to canonical shape.

### 6.4 `vo-analysis/src/project.rs`

Imports `ExtensionManifest`. The type changes shape, but analysis only reads `name` and `manifest_path`. Should adapt with minimal changes.

### 6.5 `vo-analysis/src/vfs.rs`

Uses `extension_name_from_content` which only reads `[extension].name`. No change needed.

### 6.6 `vo-web/src/module_install/extension.rs`

Uses `WasmExtensionManifest` and `WasmExtensionKind`. These types are preserved. Imports unchanged.

### 6.7 `vo-release/src/repo.rs`

Uses `include_paths_from_content`. Signature unchanged.

### 6.8 `vo-module/src/cache/install.rs`

Uses `include_paths_from_content` internally. Unchanged.

---

## 7. Test Fixtures

### 7.1 Canonical fixtures to create

Every test that writes a `vo.ext.toml` fixture must use the canonical schema.

**Native-only**:
```toml
[extension]
name = "myext"

[extension.native]
path = "rust/target/{profile}/libvo_myext"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_myext.dylib"
```

**WASM standalone**:
```toml
[extension]
name = "zip"

[extension.wasm]
type = "standalone"
wasm = "zip.wasm"
```

**WASM bindgen with native + include**:
```toml
[extension]
name = "vogui"
include = [
  "js/dist/studio_renderer.js",
  "js/dist/studio_protocol.js",
  "js/dist/studio_host_bridge.js",
]

[extension.native]
path = "rust/target/{profile}/libvo_vogui"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libvo_vogui.dylib"

[extension.wasm]
type = "bindgen"
wasm = "vogui.wasm"
js_glue = "vogui.js"
```

### 7.2 Legacy rejection tests

```rust
#[test]
fn test_reject_legacy_schema_extension_path() {
    let content = r#"
[extension]
name = "vogui"
path = "rust/target/{profile}/libvo_vogui"

[extension.wasm]
type = "standalone"
wasm = "vogui.wasm"
"#;
    let err = parse_ext_manifest_content(content, Path::new("vo.ext.toml")).unwrap_err();
    assert!(err.to_string().contains("legacy schema"));
}
```

### 7.3 Validation tests

- Missing both `[extension.native]` and `[extension.wasm]` → error
- `[extension.native]` present but no `[[extension.native.targets]]` → error
- Duplicate target triple → error
- Empty `name` → error
- `library` contains `/` → error
- `wasm` contains `/` → error
- `js_glue` present for `standalone` → error
- `js_glue` absent for `bindgen` → error

---

## 8. Implementation Order

### Phase A — New types and parser

1. Define `NativeExtensionConfig`, `NativeTargetDeclaration`, `DeclaredArtifactId`
2. Rewrite `ExtensionManifest` struct
3. Implement `parse_ext_manifest_content` with legacy rejection as the first check
4. Implement `resolve_local_native_path` as a method
5. Update `discover_extensions` to call new parser
6. Update `extension_name_from_content` — add legacy rejection
7. Write all new unit tests inside `ext_manifest.rs`

### Phase B — Consumer migration (vo-module internal)

8. Update `cache/install.rs` — likely no change since it uses `include_paths_from_content`

### Phase C — Consumer migration (external crates)

9. Update `vo-engine/src/compile/native.rs` — biggest change:
   - `prepare_native_extension_specs` must distinguish local vs published
   - For local: use `resolve_local_native_path`
   - For published frozen-build: look up `manifest.native.targets` by host target, find locked artifact path
10. Update `vo-engine/src/compile/tests/cases.rs` — rewrite fixture TOML strings
11. Update `vo-engine/src/toolchain.rs` — adapt to new `ExtensionManifest` shape
12. Update `vo-analysis/src/project.rs` — adapt to new type shape
13. Update `vo-web/src/module_install/extension.rs` — verify unchanged (uses `WasmExtensionManifest` only)

### Phase D — Real module fixtures

14. Update any real `vo.ext.toml` files in the monorepo (e.g., `vogui`, `voplay`, `vox`)

---

## 9. Risk Assessment

| Risk | Mitigation |
|---|---|
| `vo-engine` native path resolution breaks for local dev | `resolve_local_native_path` preserves existing `{profile}` and platform-suffix logic |
| `vo-web` extension loading breaks | `WasmExtensionManifest` type is preserved; `wasm_extension_from_content` still works |
| Existing `vo.ext.toml` files in the monorepo use legacy schema | Phase D explicitly updates all real fixtures |
| `extension_name_from_content` callers see errors on legacy TOML | Strict — fail fast everywhere |

---

## 10. Validation

After completing all phases:

```sh
cargo test -p vo-module --release
cargo test -p vo-engine --release
cargo test -p vo-web --release
cargo test -p vo-release --release
cargo test -p vo-analysis --release
cargo check -p vo --release
cargo check --release --target wasm32-unknown-unknown -p vo-web
```

Full integration:

```sh
./d.py test both
```

---

## 11. Deliverables

- Rewritten `lang/crates/vo-module/src/ext_manifest.rs` with canonical types and legacy rejection
- Updated consumer code in `vo-engine`, `vo-analysis`
- Updated test fixtures (both inline and real `vo.ext.toml` files)
- All tests green
