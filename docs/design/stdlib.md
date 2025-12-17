# GoX Standard Library Design

## Overview

GoX stdlib consists of **26 packages** organized into three categories based on their dependencies and implementation requirements.

## Package Categories

### Category A: Runtime Packages (2)

Packages that require deep runtime integration. They are imported like normal packages (`import "reflect"`) but can only be implemented by the language runtime, not by user code or pure GoX.

| Package | Description | Status |
|---------|-------------|--------|
| `reflect` | Runtime reflection | Planned |
| `runtime` | GC, goroutine info | Planned |

**Why special?** These packages need direct access to VM internals (type metadata, goroutine scheduler, GC state) that cannot be exposed through the normal native function interface.

### Category B: Core Packages (10)

Pure computation packages with no OS dependencies. Can be used in embedded/WASM environments (`#![no_std]` compatible).

| Package | Description | Status |
|---------|-------------|--------|
| `errors` | Error handling | ✅ |
| `strings` | String manipulation | ✅ |
| `strconv` | Type conversion | ✅ |
| `bytes` | Byte slice operations | ✅ |
| `unicode` | Character classification | ✅ |
| `math` | Mathematical functions | ✅ |
| `sort` | Sorting algorithms | ✅ |
| `encoding/json` | JSON encoding/decoding | ✅ |
| `encoding/base64` | Base64 encoding | ✅ |
| `encoding/hex` | Hexadecimal encoding | ✅ |

### Category C: Standard Packages (14)

Packages that require OS/system support or external dependencies.

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
| `regexp` | Regular expressions | ✅ |
| `sync` | Synchronization primitives | Declared |
| `context` | Context propagation | Planned |
| `net` | Networking | Declared |
| `log` | Logging | Planned |
| `flag` | Command-line flags | Planned |

## Std Modes

GoX supports two stdlib modes, similar to Rust's `no_std`:

### Core Mode (`std core`)

Only loads Category B packages (11 packages). Suitable for:
- Embedded systems
- WebAssembly
- Minimal runtime environments

### Full Mode (`std full`)

Loads all packages (Categories B + C, 24 packages). Default mode for most applications.

Note: Category A (runtime packages) are loaded on-demand regardless of mode.

## Configuration

### Project Configuration (gox.mod)

```
module myapp

std full    // Default: load all packages
// or
std core    // Minimal: core packages only
```

### Command Line Override

```bash
gox build main.gox              # Uses gox.mod setting
gox build --std=core main.gox   # Force core mode
gox build --std=full main.gox   # Force full mode
```

## Directory Structure

Import paths remain unchanged from the user's perspective (e.g., `import "strings"`). Layering is an **internal implementation detail**:

```
stdlib/
├── errors/          # [core]
├── strings/         # [core]
├── strconv/         # [core]
├── bytes/           # [core]
├── unicode/         # [core]
├── math/            # [core]
├── sort/            # [core]
├── regexp/          # [core]
├── encoding/
│   ├── json/        # [core]
│   ├── base64/      # [core]
│   └── hex/         # [core]
├── fmt/             # [std]
├── io/              # [std]
├── os/              # [std]
├── path/            # [std]
├── filepath/        # [std]
├── bufio/           # [std]
├── rand/            # [std]
├── time/            # [std]
├── sync/            # [std]
├── context/         # [std]
├── net/             # [std]
├── log/             # [std]
├── flag/            # [std]
├── reflect/         # [runtime]
└── runtime/         # [runtime]
```

Layer tags `[core]`/`[std]`/`[runtime]` are managed in package metadata or build configuration, not reflected in directory structure.

## Runtime Implementation

```rust
// crates/gox-runtime-vm/src/lib.rs

pub enum StdMode {
    Core,  // Only core packages
    Full,  // All packages (default)
}

pub fn create_vm(mode: StdMode) -> Vm {
    let mut registry = NativeRegistry::new();
    
    // Always load core packages
    natives::core::register_all(&mut registry);
    
    // Full mode also loads std packages
    if mode == StdMode::Full {
        natives::std::register_all(&mut registry);
    }
    
    Vm::with_natives(registry)
}
```

## Import Behavior

Import paths remain unchanged regardless of mode:

```go
import "strings"    // Works in both modes
import "os"         // Only works in full mode
```

If a package is imported but not available in the current mode, the compiler will report an error:

```
error: package "os" requires std=full mode
  --> main.gox:3:8
  |
3 | import "os"
  |        ^^^^
```

## Package Implementation Status

| Status | Count | Packages |
|--------|-------|----------|
| ✅ Complete | 17 | strings, strconv, bytes, unicode, math, sort, regexp, encoding/*, fmt, io, os, path, rand, time, errors |
| 📝 Declared | 4 | bufio, sync, net |
| ❌ Planned | 5 | reflect, runtime, filepath, context, log, flag |

## Future Considerations

1. **Runtime packages**: `reflect` and `runtime` require significant VM changes
2. **Network stack**: `net` package needs async I/O support
3. **Sync primitives**: `sync` needs goroutine/channel integration
