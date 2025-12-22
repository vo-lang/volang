# GoX Standard Library

## Overview

GoX stdlib contains **26 packages** organized in three layers:

| Layer | Characteristics | Count |
|-------|-----------------|-------|
| **core** | No OS dependency, usable in embedded/WASM | 11 |
| **std** | Requires OS support | 13 |
| **runtime** | Requires runtime internals | 2 |

## Function Implementation Types

Stdlib functions are declared in GoX source (`stdlib/*/xxx.gox`) with three implementation types:

```
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│    Pure GoX     │  │   GoX + extern  │  │   Pure extern   │
│                 │  │                 │  │   (no body)     │
│ func F() {      │  │ func F() {      │  │                 │
│     // GoX code │  │     // GoX code │  │ func F()        │
│ }               │  │     extern_f()  │  │                 │
│                 │  │ }               │  │                 │
│ e.g. HasPrefix  │  │ e.g. Contains   │  │ e.g. Index      │
└─────────────────┘  └─────────────────┘  └─────────────────┘
```

### Syntax

```gox
// Pure GoX — fully implemented in GoX
func HasPrefix(s, prefix string) bool {
    return len(s) >= len(prefix) && s[:len(prefix)] == prefix
}

// GoX + extern — GoX logic calling extern functions
func Contains(s, substr string) bool {
    return Index(s, substr) >= 0
}

// Pure extern — no body, implemented by runtime
func Index(s, substr string) int
func ToLower(s string) string
```

## Design Principles

### 1. GoX-First

Prefer GoX implementation. Use `extern` only when:
- **Cannot implement in GoX**: syscalls, Unicode tables, runtime internals
- **Performance critical**: string search, regex, sorting algorithms

### 2. Extern Implementation Architecture

```
extern func Index(s, substr string) int
                       │
     ┌─────────────────┴─────────────────┐
     ▼                                   ▼
┌─────────────────────┐       ┌─────────────────────┐
│  gox-runtime-vm     │       │ gox-runtime-native  │
│  (VM Binding)       │       │ (AOT C ABI)         │
└─────────┬───────────┘       └─────────┬───────────┘
          │                             │
          └──────────┬──────────────────┘
                     ▼
          ┌─────────────────────┐
          │  gox-runtime-core   │
          │  (Pure Logic)       │
          └─────────────────────┘
```

#### Layer Responsibilities

| Layer | Crate | Responsibility |
|-------|-------|----------------|
| **Core** | `gox-runtime-core` | Pure business logic, no GC/VM dependency |
| **VM Binding** | `gox-runtime-vm` | `ExternCtx` wrapper for VM interpreter |
| **AOT Binding** | `gox-runtime-native` | `extern "C"` wrapper for Cranelift AOT |

#### Example: `strings.Index`

**Step 1: Core Layer** — Pure logic, works with Rust types

```rust
// gox-runtime-core/src/stdlib/strings.rs
pub fn index(s: &str, substr: &str) -> i64 {
    s.find(substr).map(|i| i as i64).unwrap_or(-1)
}
```

**Step 2a: VM Binding** — Unwrap from `ExternCtx`, call Core, return result

```rust
// gox-runtime-vm/src/stdlib/strings.rs
pub fn extern_index(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    let result = gox_runtime_core::stdlib::strings::index(s, substr);
    ctx.ret_i64(0, result);
    ExternResult::Ok(1)
}
```

**Step 2b: AOT Binding** — C ABI wrapper for Cranelift

```rust
// gox-runtime-native/src/ffi/strings.rs
#[no_mangle]
pub unsafe extern "C" fn gox_strings_index(s: GcRef, substr: GcRef) -> i64 {
    let s_str = gox_runtime_core::objects::string::as_str(s);
    let substr_str = gox_runtime_core::objects::string::as_str(substr);
    gox_runtime_core::stdlib::strings::index(s_str, substr_str)
}
```

#### Why This Architecture?

1. **Code Reuse**: Core logic written once, used by both VM and AOT
2. **Testability**: Core layer can be unit tested without VM/GC
3. **no_std Support**: Core layer can be `#![no_std]` compatible
4. **Clear Boundaries**: Each layer has single responsibility

## Package List

### Core Layer (11 packages, no OS dependency)

| Package | Description | Status |
|---------|-------------|--------|
| `errors` | Error handling | ✅ |
| `strings` | String manipulation | ✅ |
| `strconv` | Type conversion | ✅ |
| `bytes` | Byte slice operations | ✅ |
| `unicode` | Character classification | ✅ |
| `math` | Mathematical functions | ✅ |
| `sort` | Sorting algorithms | ✅ |
| `regexp` | Regular expressions | ✅ |
| `encoding/json` | JSON encoding/decoding | ✅ |
| `encoding/base64` | Base64 encoding | ✅ |
| `encoding/hex` | Hexadecimal encoding | ✅ |

### Std Layer (13 packages, requires OS)

| Package | Description | Status |
|---------|-------------|--------|
| `fmt` | Formatted I/O | ✅ |
| `io` | I/O interfaces | ✅ |
| `os` | OS operations | ✅ |
| `path` | Path manipulation | ✅ |
| `filepath` | File path operations | Planned |
| `bufio` | Buffered I/O | Declared |
| `rand` | Random numbers | ✅ |
| `time` | Time operations | ✅ |
| `sync` | Synchronization primitives | Declared |
| `context` | Context propagation | Planned |
| `net` | Networking | Declared |
| `log` | Logging | Planned |
| `flag` | Command-line flags | Planned |

### Runtime Layer (2 packages, requires runtime internals)

| Package | Description | Status |
|---------|-------------|--------|
| `reflect` | Runtime reflection | Planned |
| `runtime` | GC, goroutine info | Planned |

## Std Mode

Similar to Rust's `no_std`, GoX supports two modes:

| Mode | Available Packages | Use Case |
|------|-------------------|----------|
| `std core` | core layer only | Embedded, WASM |
| `std full` | core + std | Default |

```
// gox.mod
module myapp
std full    // or: std core
```

```bash
gox build main.gox              # Uses gox.mod setting
gox build --std=core main.gox   # Force core mode
```

## Rust Implementation Examples

### Core Layer (pure logic)

```rust
// gox-runtime-core/src/stdlib/strings.rs
pub fn index(s: &str, substr: &str) -> i64 {
    s.find(substr).map(|i| i as i64).unwrap_or(-1)
}
```

### VM Binding

```rust
// gox-runtime-vm/src/stdlib/strings.rs
fn extern_index(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    let substr = ctx.arg_str(1);
    ctx.ret_i64(0, gox_runtime_core::stdlib::strings::index(s, substr));
    ExternResult::Ok(1)
}
```

### C ABI (AOT)

```rust
// gox-runtime-core/src/ffi.rs
#[no_mangle]
pub unsafe extern "C" fn gox_strings_index(s: GcRef, substr: GcRef) -> i64 {
    crate::stdlib::strings::index(string::as_str(s), string::as_str(substr))
}
```

## Adding New Functions

```
Can it be implemented in GoX?
  ├─ Yes → Implement in stdlib/pkg/pkg.gox
  └─ No  → Needs syscalls or GC access?
            ├─ Yes → Implement in VM layer
            └─ No  → Implement in Core layer + VM binding
```

### Step 1: GoX Declaration

```gox
// stdlib/strings/strings.gox

// Pure GoX
func NewFunc(s string) bool {
    return len(s) > 0
}

// Or extern (no body)
func NewExternFunc(s string) int
```

### Step 2: Core Layer (if extern)

```rust
// gox-runtime-core/src/stdlib/strings.rs
pub fn new_extern_func(s: &str) -> i64 {
    // Pure logic, no GC dependency
}
```

### Step 3: VM Binding (if extern)

```rust
// gox-runtime-vm/src/stdlib/strings.rs
fn extern_new_extern_func(ctx: &mut ExternCtx) -> ExternResult {
    let s = ctx.arg_str(0);
    ctx.ret_i64(0, gox_runtime_core::stdlib::strings::new_extern_func(s));
    ExternResult::Ok(1)
}

// Register
registry.register("strings.NewExternFunc", extern_new_extern_func);
```

### Step 4: C ABI (optional, for AOT)

```rust
// gox-runtime-core/src/ffi.rs
#[no_mangle]
pub unsafe extern "C" fn gox_strings_new_extern_func(s: GcRef) -> i64 {
    crate::stdlib::strings::new_extern_func(string::as_str(s))
}
