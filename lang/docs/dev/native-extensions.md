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
lists with `export_extensions!(...)`.

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

```rust
use vo_ext::prelude::*;

#[vo_fn("math", "FastAdd")]
fn fast_add(a: i64, b: i64) -> i64 {
    a + b
}

vo_ext::export_extensions!();
```

## Crate Boundaries

- `vo-ext`: user-facing SDK and `#[vo_fn]` re-export
- `vo-ffi-macro`: `#[vo_fn]` and `#[vostd_fn]` proc macros
- `vo-runtime`: `ExternCallContext`, `ExternResult`, extension ABI structs,
  ABI version/fingerprint, and native loader support
- `vo-module`: parses extension metadata from `vo.mod`
- `vo-release`: stages `vo.release.json`, `vo.web.json`, and declared artifacts

## ABI Compatibility

Native extensions export `vo_ext_get_entries()` and
`vo_ext_get_abi_fingerprint()`. The runtime checks the ABI version and
fingerprint before using the returned `ExtensionTable`.

`ExtensionTable` currently contains:

```rust
pub struct ExtensionTable {
    pub version: u32,
    pub entry_count: u32,
    pub entries: *const ExternEntry,
}
```

## Current Limits

- Published native artifacts are target-specific and must match the declared
  `[[extension.native.targets]]` entries.
- Published dependencies do not fall back to building native extension
  artifacts from source during frozen compilation.
- Extension ABI stability is not promised across runtime ABI changes; rebuild
  extensions when the ABI version or fingerprint changes.
