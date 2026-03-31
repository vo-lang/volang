# Module Cache Validation Redesign

**Date**: 2026-03-31
**Status**: Implementation in progress

## Problem

`vo-module`, `vo-engine`, and `vo-web` each independently implement "validate that
an installed module matches its `LockedModule` metadata".  The three copies exist
because `vo-module::materialize` hardcodes `std::fs` for validation and only
exposes `is_source_cached() -> bool`, which loses all error detail.

| Function | Crate | I/O backend |
|----------|-------|-------------|
| `materialize::validate_source_cache_entry` (private) | vo-module | `std::fs` |
| `native::validate_installed_module` | vo-engine | `std::fs` (duplicate) |
| `module_install::validate_vfs_installed_module` | vo-web | JS VFS (duplicate) |

The same duplication pattern applies to helper readers:

- `read_trimmed_metadata` — private in both vo-module and vo-engine
- `read_vfs_installed_version` / `read_vfs_installed_source_digest` /
  `read_vfs_release_manifest_digest` — reimplemented in vo-web for VFS

### Root cause

`vo-module` has no **FileSystem-generic abstraction for installed module state**.
It mixes pure cache-layout helpers with native-only download/validation I/O in a
single `materialize.rs` module.

### Secondary issue

`vo-engine::toolchain::install_module` constructs a `LockedModule` with an
incorrect `release_manifest` field (set to `source.digest` instead of the SHA-256
of the raw manifest bytes).  This is a semantic bug that would cause frozen-build
validation failures if the `LockedModule` were persisted.

## Design

### 1. Extend `FileSystem` trait with `read_bytes`

```rust
// vo-common/src/vfs.rs
pub trait FileSystem: Send + Sync {
    fn read_file(&self, path: &Path) -> io::Result<String>;
    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>>;  // NEW
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;
    fn exists(&self, path: &Path) -> bool;
    fn is_dir(&self, path: &Path) -> bool;
    fn root(&self) -> Option<&Path> { None }
}
```

Implementations: `RealFs` (`std::fs::read`), `MemoryFs` (`.as_bytes().to_vec()`),
`ZipFs` (`.as_bytes().to_vec()`), `OverlayFs` (delegate), `WasmVfs` (delegate to
JS VFS).

### 2. New `vo-module/src/cache/` module

```
cache/
  mod.rs          — re-exports
  layout.rs       — cache_key, cache_dir, relative_module_dir, VERSION_MARKER,
                    SOURCE_DIGEST_MARKER  (moved from materialize.rs)
  inspect.rs      — generic readers: read_installed_version<F>,
                    read_installed_source_digest<F>, read_installed_manifest_digest<F>
  validate.rs     — validate_installed_module<F>, validate_installed_artifact<F>,
                    InstalledModuleError
```

#### `InstalledModuleError`

Lives in `cache/validate.rs`.  Structured enough for both CLI and IDE consumers:

```rust
pub struct InstalledModuleError {
    pub module: String,
    pub version: String,
    pub field: InstalledModuleField,
    pub kind: InstalledModuleErrorKind,
    pub detail: String,
}

pub enum InstalledModuleField {
    Directory,
    ModFile,
    VersionMarker,
    SourceDigest,
    ReleaseManifest,
    Artifact,
}

pub enum InstalledModuleErrorKind {
    Missing,
    Mismatch { expected: String, found: String },
    ParseFailed,
}
```

#### `validate_installed_module`

```rust
pub fn validate_installed_module<F: FileSystem>(
    fs: &F,
    module_dir: &Path,
    locked: &LockedModule,
) -> Result<(), InstalledModuleError>
```

Checks (in order):
1. `module_dir` exists and is a directory
2. `vo.mod` exists, parses, module path matches, toolchain constraint matches
3. `.vo-version` content matches `locked.version`
4. `.vo-source-digest` content matches `locked.source`
5. `vo.release.json` bytes SHA-256 matches `locked.release_manifest`

### 3. Slim down `materialize.rs`

- Move `cache_key`, `cache_dir`, `relative_module_dir`, `cache_relative_dir`,
  `VERSION_MARKER`, `SOURCE_DIGEST_MARKER` to `cache::layout`.
- Re-export them from `materialize` for backward compatibility.
- Replace internal `validate_source_cache_entry` call with
  `cache::validate::validate_installed_module(RealFs, ...)`.
- `is_source_cached` delegates to the new function.

### 4. Simplify `vo-engine/src/compile/native.rs`

Delete:
- `read_trimmed_metadata`
- `installed_module_version`
- `installed_module_source_digest`
- `installed_module_release_manifest_digest`
- `validate_installed_module` (the 228-line reimplementation)

Replace with:
```rust
fn validate_locked_module_installed(locked: &LockedModule, mod_root: &Path) -> Result<(), CompileError> {
    let dir = vo_module::cache::layout::cache_dir(mod_root, &locked.path, &locked.version);
    let fs = RealFs::new(&dir);
    vo_module::cache::validate::validate_installed_module(&fs, Path::new("."), locked)
        .map_err(|e| /* convert InstalledModuleError → ModuleSystemError */)
}
```

### 5. Simplify `vo-web/src/module_install.rs`

Delete:
- `read_vfs_installed_version`
- `read_vfs_installed_source_digest`
- `read_vfs_release_manifest_digest`
- `validate_vfs_installed_module` (the 68-line reimplementation)

Replace with a thin wrapper calling
`cache::validate::validate_installed_module(&WasmVfs, ...)`.

### 6. Fix `toolchain::install_module` digest bug

Change `release_manifest: manifest.source.digest.clone()` to use the actual
SHA-256 of the raw manifest bytes.  This requires `registry.fetch_manifest_raw()`
or computing the digest from `serde_json::to_vec(&manifest)`.

## What does NOT change

- `ProjectDeps` / `read_project_deps` — already FileSystem-generic
- `vo-web::wasm_fetch` — platform-specific browser Fetch API
- `vo-engine` native extension building (`cargo build`)
- `vo-web` WASM extension loading
- `vo-ext` host bridge
- Compile cache fingerprinting

## Validation plan

```sh
cargo test -p vo-module --release
cargo test -p vo-engine --release
cargo test -p vo-web --release
cargo check -p vo --release
cargo check --release --target wasm32-unknown-unknown -p vo-web
```
