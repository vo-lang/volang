# Vo Native Extensions

This document describes the native extension system for Vo, which allows Vo libraries to include Rust implementations for performance-critical functions.

## Overview

Similar to Python's C extensions, Vo supports native extensions written in Rust. Extensions are:
- Compiled as dynamic libraries (`.so` on Linux, `.dylib` on macOS, `.dll` on Windows)
- Declared in `vo.mod` extension metadata
- Loaded at runtime by the Vo CLI

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         vo (executable)                          │
│  ┌─────────────┐  ┌──────────────┐  ┌─────────────────────────┐ │
│  │  vo-cli     │  │  vo-runtime  │  │  stdlib natives         │ │
│  │  (frontend) │  │  (GC + FFI)  │  │  (linkme registered)    │ │
│  └─────────────┘  └──────────────┘  └─────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
                              │
                    dlopen at runtime
                              ▼
              ┌──────────────────────────────────────┐
              │       User Native Extensions         │
              │       (libmyext.so/.dylib/.dll)      │
              │   ┌────────────────────────────────┐ │
              │   │ #[vo_extern("pkg", "Func")]    │ │
              │   │ fn my_func(...) {...}          │ │
              │   └────────────────────────────────┘ │
              └──────────────────────────────────────┘
```

## Creating an Extension

### 1. Project Structure

Standard Vo library structure with native support:

```
mylib/
├── libmylib.so          # Native binary (optional, platform-specific)
├── rust/                # Rust source
│   ├── Cargo.toml
│   └── src/
│       └── lib.rs       # Native implementation
├── vo.mod               # module myorg/mylib
├── mylib.vo             # Vo interface (extern declarations)
├── examples/
│   └── hello.vo
```

### 2. Vo Interface (`mylib.vo`)

Declare extern functions (no body = native implementation required):

```go
package math

// Native function - must be implemented in Rust
func FastAdd(a, b int) int

// Pure Vo function - no native needed
func SlowAdd(a, b int) int {
    return a + b
}
```

### 3. Extension Metadata in `vo.mod`

Declare extension metadata in the module manifest:

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

### 4. Rust Implementation

**Cargo.toml:**

```toml
[package]
name = "myext-native"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
vo-ext = { path = "path/to/vo-ext" }  # Or from crates.io
```

**src/lib.rs:**

```rust
use vo_ext::prelude::*;

#[vo_extern("myext/math", "FastAdd")]
fn fast_add(a: i64, b: i64) -> i64 {
    a + b
}

// Required: export the extension table
vo_ext::export_extensions!();
```

### 5. Build

```bash
cd native
cargo build --release
```

## Using Extensions

Extensions are automatically discovered from the library's root directory when the Vo program imports packages from that library.

```go
// main.vo
package main

import "myext/math"

func main() {
    println(math.FastAdd(1, 2))  // Calls native Rust code
}
```

Run with:

```bash
vo run main.vo
```

## Type Mapping

| Vo Type | Rust Type |
|---------|-----------|
| `int` | `i64` |
| `int8` | `i8` |
| `int16` | `i16` |
| `int32` | `i32` |
| `int64` | `i64` |
| `uint` | `u64` |
| `uint8` | `u8` |
| `uint16` | `u16` |
| `uint32` | `u32` |
| `uint64` | `u64` |
| `float32` | `f32` |
| `float64` | `f64` |
| `bool` | `bool` |
| `string` | `&str` (arg) / `String` (ret) |
| `[]byte` | `&[u8]` (arg) / `Vec<u8>` (ret) |

## Crate Structure

- **vo-ext**: User-facing SDK for creating extensions
- **vo-runtime**: Contains `ext_loader` module for loading extensions
- **vo-ffi-macro**: Proc macro for `#[vo_extern]` attribute

## ABI Compatibility

Extensions export a `vo_ext_get_entries()` function that returns an `ExtensionTable` with:
- ABI version (must match runtime)
- Pointers to extern function tables

If the ABI version doesn't match, the extension won't load.

## Limitations

1. **No hot reload**: Extensions are loaded once at startup
2. **Platform-specific**: Must build for each target platform
3. **ABI stability**: Extensions must be rebuilt when upgrading Vo
