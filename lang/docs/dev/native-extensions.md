# Vo Native Extensions

This document summarizes the current native extension shape. For Rust API
details, see `lang/docs/dev/native-ffi.md`; for the module metadata contract,
see `lang/docs/spec/native-ffi.md`.

## Overview

Current Rust-backed extensions are:

- declared in `vo.mod` under `[extension]` and `[extension.native]`
- implemented with `#[vo_fn]` from `vo-ext` / `vo-ffi-macro`
- exported with `vo_ext::export_extensions!()`
- loaded by the runtime from dynamic libraries on native targets

WASM extension builds use the same `vo-ext` crate but export explicit entry
lists with `export_extensions!(vo_extension_entry!(...))`. The expression
macro accepts compile-time string literals and resolves the same canonical
module/package/function identity as `#[vo_fn]`; callers never spell a generated
Rust symbol.

## Project Shape

```text
mylib/
├── rust/
│   ├── Cargo.toml
│   └── src/lib.rs
├── vo.mod
└── math.vo
```

`math.vo` declares the Vo interface. A function declaration without a body is
treated as extern by the parser:

```vo
package math

func FastAdd(a int, b int) int;
```

`vo.mod` declares extension metadata:

```toml
module github.com/example/mylib
vo ^0.1.0

[extension]
name = "mylib"

[extension.native]
path = "rust/target/{profile}/libmylib"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libmylib.dylib"
```

The Rust crate exports the native table:

```toml
# rust/Cargo.toml
[package.metadata.vo]
vomod = "../vo.mod"
```

```rust
use vo_ext::prelude::*;

#[vo_fn("mylib", "FastAdd")]
fn fast_add(a: i64, b: i64) -> i64 {
    a + b
}

vo_ext::export_extensions!();
```

The macro resolves `mylib` against `vo.mod` and records the canonical owner
`github.com/example/mylib`. It validates the matching bodyless Vo declaration
at compile time and embeds source dependency markers so editing the declaration
invalidates the Rust expansion. The extension display name from
`[extension].name` remains provider metadata and never participates in extern
identity.

`export_extensions!()` also emits one independent process-local owner record.
That record exists even when the crate contains no `#[vo_fn]`, so a loaded
child module with an empty function table still reserves its exact namespace.
Invoke the macro once in the final extension crate; a repeated invocation is a
compile-time error.

The generated dependency markers cover `Cargo.toml`, the configured `vo.mod`,
a selected `vo.work`, each selected workspace override's `vo.mod`, and every
`.vo` source file consulted during expansion. They also observe `VOWORK` and
`VO_FFI_SOURCE_FINGERPRINT` as compile-time inputs.

Use `vo build .` from the module root for the supported local build path.
Every native extension commits the `Cargo.lock` at its actual Cargo workspace
root (`rust/Cargo.lock` for a standalone extension crate). The engine discovers
that workspace through Cargo, runs metadata and builds with `--locked`, and
rejects a missing or concurrently changed workspace lock. It never creates or
restores a member-local substitute lock.
vo-engine and other Volang-owned producers, including the Quickplay builders,
hash their complete declared Rust and Vo inputs, manifest and lock files,
active workspace overrides, every reachable local Cargo package, and
Cargo/toolchain/configuration build context. They inject a content-and-generation
token into the entire reachable Rust compilation graph and expose it through
`VO_FFI_SOURCE_FINGERPRINT`; additions, removals, modifications, and A-B-A
input transitions therefore force recompilation and invalidate cached
artifacts. vo-engine verifies the same input generation after Cargo finishes
and retries with a fresh token when the source snapshot moved during the build.

The engine executes Cargo even when its own native marker is current, preserving
Cargo and `build.rs` `rerun-if-changed` authority. It consumes Cargo's JSON
`compiler-artifact` message for the exact package and `cdylib` target and
requires that canonical artifact to equal the path declared by `vo.mod`; a
stale file at the declared hint cannot satisfy the build.

Direct `cargo build` remains a low-level integration path. Its caller must set
`VO_FFI_SOURCE_FINGERPRINT` to a new deterministic content fingerprint when
relevant source membership changes, or run `cargo clean` before rebuilding.
This closes stable Rust's proc-macro directory-membership gap while preserving
normal incremental tracking for files already consulted by the macro.

## Crate Boundaries

- `vo-ext`: user-facing SDK and `#[vo_fn]` re-export
- `vo-ffi-macro`: `#[vo_fn]` and `#[vostd_fn]` proc macros
- `vo-stdlib-source`: publishable `stdlib.toml` and `.vo` assets rooted at
  `lang/stdlib`; proc macros and embedded stdlib consumers share this one
  canonical source tree
- `vo-runtime`: `ExternCallContext`, `ExternResult`, extension ABI structs,
  ABI version/fingerprint, and native loader support
- `vo-module`: parses extension metadata from `vo.mod`
- `vo-release`: stages `vo.release.json`, `vo.web.json`, and declared artifacts

## ABI Compatibility

Native extensions export `vo_ext_get_entries()` and
`vo_ext_get_abi_fingerprint()`, along with `vo_ext_get_abi_version()`. The
runtime checks the ABI version and fingerprint before using the returned
`ExtensionTable`.

Each entry name uses the typed `vo1` length-prefixed package/function codec.
The loader rejects malformed, duplicate, legacy flattened, or out-of-owner
entries as one invalid table.

`ExtensionTable` currently contains:

```rust
#[repr(C)]
pub struct ExtensionTable {
    pub version: u32,
    pub entry_count: u32,
    pub entries: *const ExternEntry,
}

#[repr(C)]
pub struct ExternEntry {
    pub name_ptr: *const u8,
    pub name_len: u32,
    pub module_owner_ptr: *const u8,
    pub module_owner_len: u32,
    pub func: Option<ExternFnPtr>,
    pub effects_bits: u64,
}
```

Every entry embeds the exact canonical `ModulePath` from its authoritative
`vo.mod`. The loader rejects the complete table when an entry owner differs
from the selected extension artifact owner, when a decoded package falls
outside that owner, or when either byte range is malformed. The ABI
fingerprint covers the size, alignment, and every field offset of both
`ExternEntry` and `ExtensionTable`, including the nullable function-pointer
representation.

At VM load, linkme and dynamic providers contribute to one deduplicated owner
catalog. Routing selects the deepest owner boundary before exact function
lookup, so a deeper provider that omits a function blocks parent fallback.
The active result is independent of provider load order, and one exact owner
cannot be split across linkme and a dynamic artifact.

Process-local providers contribute owner records through
`EXTERN_MODULE_OWNER_TABLE`, independently from function entries. Invalid or
duplicate owner records and entries whose owner was never declared reject the
complete process-local catalog transactionally.

## Current Limits

- Published native artifacts are target-specific and must match the declared
  `[[extension.native.targets]]` entries.
- Published dependencies do not fall back to building native extension
  artifacts from source during frozen compilation.
- Extension ABI stability is not promised across runtime ABI changes; rebuild
  extensions when the ABI version or fingerprint changes.
