# Vo Module Specification (Transitive Closure Model)

Version: 1.1  
Status: Draft

## 1. Overview

Vo uses a deterministic module system with pinned versions and offline builds.
Dependencies are declared in `vo.mod`, and transitive dependencies are discovered by recursively reading each dependency's own `vo.mod`. There is no version solving: any version disagreement is a hard error.

**Core Properties:**

- Exact versions only (including optional pre-release/build metadata)
- `vo build` never accesses the network
- Transitive dependency closure is computed from `vo.mod` files
- A module path may appear with only one version in the entire closure

## 2. Module Definition

### 2.1 The `vo.mod` File

Every module has a `vo.mod` at its root containing:

- Exactly one `module` line
- Exactly one `vo` line
- Zero or more `require` lines (direct dependencies)

**Format:**

```
module <module-path>
vo <toolchain-version>

require <alias> <module-path> <version>
require <alias> <module-path> <version>
...
```

**Example:**

```
module github.com/myuser/myproject
vo 0.1

require bar github.com/foo/bar v1.2.3
require qux github.com/baz/qux v0.1.0
```

`require` format is normative: `require <alias> <module-path> <version>`.
External imports use alias form `@"alias"`.

### 2.2 Module Path

A module path is a slash-separated path such as `github.com/user/project`.
It is the import prefix for packages inside the module.

### 2.3 Version Format

Versions are exact and must be syntactically SemVer-like:

- `vMAJOR.MINOR.PATCH`
- Optional pre-release: `-...` (e.g., `v2.0.0-beta.1`)
- Optional build metadata: `+...` (e.g., `v1.2.3+meta`)

No ranges, no constraints, no "latest".

## 3. Project and Cache Layout

A typical project:

```
myproject/
├── vo.mod
├── vo.sum
├── main.vo
├── util/
│   └── helper.vo
└── .vodeps/
    └── github.com/
        └── foo/
            └── bar@v1.2.3/
                ├── vo.mod
                └── ...
```

**Rules:**

- `.vodeps/` is a local cache; it is not committed to VCS.
- `vo.sum` is committed to VCS and records dependency checksums.
- Each cached dependency module lives at: `.vodeps/<module-path>@<version>/`
- The cached module root must contain its own `vo.mod`.

### 3.1 Global Cache

Implementations may additionally use a global cache (for example `~/.vo/mod`).
When both project-local and global caches are available, tooling must behave deterministically and enforce the same version/checksum rules.

## 4. Standard Library

- Standard library packages are imported with the reserved prefix `std/`, e.g.:
  - `import "std/io"`
  - `import "std/strings"`
- The compiler manages the standard library location internally.
- No user or third-party module may define packages under the `std/` prefix.

## 5. Dependency Closure and Version Consistency

### 5.1 Closure Computation

Given a root module R:

1. Read `R/vo.mod`
2. For each `require A M V`, load module `M@V` from cache and read `vo.mod`
3. Recursively repeat for each loaded module's `require` lines
4. Maintain a visited set to avoid infinite recursion

This produces a **module closure**: a set of `(module-path -> version)` pairs.

### 5.2 Version Conflict Rule (No Solver)

A module path may appear at **only one version** across the entire closure.

- If the closure requires both `github.com/foo/bar v1.2.3` and `github.com/foo/bar v1.2.4`, that is a **hard error**.
- The toolchain must report the conflict and show at least one dependency chain for each side of the conflict.

### 5.3 Offline Build Rule

`vo build --offline` must not download anything.

- If any required module directory is missing from cache, the build fails.
- The error should indicate the missing `(module, version)` and suggest `vo get`.

### 5.4 Lock/Checksum Rule (`vo.sum`)

- `vo.sum` stores checksums for fetched dependencies.
- `vo get` and `vo sync` update `vo.sum`.
- Build/check must verify checksum when lock data exists.
- A checksum mismatch is a hard error.

## 6. Imports and Resolution

### 6.1 Import Syntax

Vo uses the `@` symbol to distinguish external dependencies from local/standard library packages:

```go
import (
    // Standard library (no marker)
    "fmt"
    "strings"
    
    // Local project packages (no marker, relative to project root)
    "utils"
    "handlers/api"
    
    // External dependencies (@ marker, must be declared in vo.mod)
    @"gin"
    @"jwt"
)
```

**Supported import forms:**

```go
import "path"              // Standard library or local package
import @"alias"            // External dependency (defined in vo.mod)
import name "path"         // With alias
import name @"alias"       // External dependency with alias
import . "path"            // Dot import (import all exported names)
import _ "path"            // Blank import (init only)
```

### 6.2 Dependency Declaration in vo.mod

```
module myproject

require gin github.com/gin-gonic/gin v1.9.0
require jwt github.com/golang-jwt/jwt v5.0.0
```

Format: `require <alias> <module-path> <version>`

- **alias**: The name used in source code with `@"xxx"`
- **module-path**: The actual repository URL
- **version**: Exact version number

### 6.3 Resolution Algorithm

For an import path `P`:

1. **External dependency** (`@"P"` form)
   - Look up alias `P` in the vo.mod require list
   - If found: map to `.vodeps/<module-path>@<version>/`
   - If not found: error (undeclared external dependency)

2. **Standard library** (`"P"` where P is a known stdlib package)
   - Resolve to the compiler's internal standard library location
   - Known stdlib packages: `fmt`, `os`, `io`, `strings`, `strconv`, `math`, `time`, `sync`, `context`, `errors`, `log`, `sort`, `bytes`, `bufio`, `encoding`, `net`, `path`, `regexp`, `reflect`, `runtime`, `testing`, etc.

3. **Local project package** (`"P"` where P is not stdlib)
   - Look for directory `<project-root>/<P>/`
   - Supports nesting: `"handlers/api"` → `./handlers/api/`
   - If not found: error

### 6.4 Projects Without vo.mod

The `vo.mod` file is optional. When a project has no `vo.mod`:

- `@"xxx"` will fail (no alias definitions to look up)
- `"xxx"` works normally: resolves as stdlib → project directory
- Suitable for simple projects with no external dependencies

No special code handling is required—the resolution algorithm naturally handles this case.

### 6.5 Example

**Project structure:**
```
myproject/
├── vo.mod
├── main.vo
├── utils/
│   └── strings.vo
└── handlers/
    └── api/
        └── handler.vo
```

**vo.mod:**
```
module myproject

require gin github.com/gin-gonic/gin v1.9.0
require validator github.com/go-playground/validator v10.0.0
```

**main.vo:**
```go
package main

import (
    "fmt"              // Standard library
    "utils"            // Local: ./utils/
    "handlers/api"     // Local: ./handlers/api/
    
    @"gin"             // External: github.com/gin-gonic/gin
    @"validator"       // External: github.com/go-playground/validator
)
```

### 6.6 Advantages of This Design

| Benefit | Description |
|---------|-------------|
| **Clear distinction** | `@` marker instantly shows external vs local |
| **No URL in source** | Library migration only requires changing vo.mod |
| **No ambiguity** | stdlib `fmt` and external `@"fmt"` (if any) don't conflict |
| **Compile-time check** | `@"xxx"` must be declared in vo.mod |
| **IDE friendly** | Easy to implement syntax highlighting and autocomplete |

## 7. Package Rules

These rules define how directories form packages; they are required for deterministic builds.

1. A directory corresponds to a single package.
2. All `.vo` files in the same directory must declare the same `package <name>`.
3. Package name matching the directory name is **recommended** but not required.
4. Multiple files in a directory are compiled together as one package.
5. Files are processed in **alphabetical order** by filename.

### 7.1 The `main` Package

- A package named `main` with a `func main()` is an executable entry point.
- The `main` package cannot be imported by other packages.

### 7.2 Test Files

Files with the `_test.vo` suffix are only compiled during testing:

```
util/
├── helper.vo       # Always compiled
└── helper_test.vo  # Only compiled for tests
```

Test files may declare either:

- `package util` - Internal tests (access to unexported symbols)
- `package util_test` - External tests (black-box testing)

## 8. Internal Packages

Vo supports Go-style internal visibility:

- Any package whose import path contains `/internal/` is restricted.
- Let the "internal parent" be the path segment immediately before `/internal/`.
- An internal package may be imported only by packages whose import paths share that internal parent as a prefix.

**Example:**

- Module: `github.com/myuser/myproject`
- Internal package: `github.com/myuser/myproject/internal/secret`
- Allowed importers:
  - `github.com/myuser/myproject/cmd/tool`
  - `github.com/myuser/myproject/other`
- Disallowed:
  - Any package in another module
  - `github.com/other/project/...`

## 9. Initialization

### 9.1 Package-Level Variables

Package-level variable initialization follows dependency order:

```go
var a = b + 1  // Initialized second
var b = 2      // Initialized first
```

The compiler analyzes dependencies to determine initialization order.

### 9.2 The `init` Function

Packages may declare one or more `init` functions:

```go
func init() {
    // Initialization code
}
```

**Execution Order:**

1. All imported packages are initialized first (in dependency order)
2. Package-level variables are initialized
3. `init` functions are called in order:
   - Files processed in alphabetical order by filename
   - Within a file, `init` functions called in declaration order

## 10. Cycles

- **Import cycles** (package-level) are not allowed; they are compile-time errors.
- **Module dependency cycles** are handled by the visited set during closure computation; the closure computation must terminate.

## 11. Visibility

### Export Rules

- **Uppercase** first letter: Exported (public)
- **Lowercase** first letter: Unexported (package-private)

```go
package util

func PublicFunc() {}   // Exported - accessible from other packages
func privateFunc() {}  // Unexported - only accessible within util package

type PublicType struct {
    PublicField  int    // Exported field
    privateField int    // Unexported field
}
```

## 12. Build Constraints

Build constraints allow conditional compilation based on tags:

```go
//go:build linux

package mypackage
```

Files are included only when the build tags match.

### 12.1 Platform Build Tags

The two primary build tags for platform layering are `native` and `wasm`.

Library authors use per-file build tags to provide platform-specific implementations:

```
mylib/
├── add.vo            // shared types and pure Vo functions (no tag)
├── add_native.vo     //go:build native  — extern declarations + native implementation
└── add_wasm.vo       //go:build wasm    — pure Vo fallback
```

**Key rule:** An `extern func` (a function declaration with no body) requires a concrete implementation for every target the module is built for. If a file with `extern func` declarations is included for a given target but no implementation is registered (via `vo.ext.toml` for native, or host bindings for wasm), the build fails.

This means:

- **Native-only library** (no WASM support): `extern func` in non-guarded or `//go:build native` files, no `//go:build wasm` file. WASM build fails at compile time — correct behavior.
- **Cross-platform library**: `//go:build native` file with `extern func`, `//go:build wasm` file with pure Vo body. Both targets compile.
- **Pure Vo library** (no extern): works on every target with no extra files.

The toolchain is the sole arbiter of platform support: if it compiles for a target, it is supported.

## 13. CLI Commands

### 13.1 `vo init <module-path>`

Creates a new `vo.mod` in the current directory:

```bash
$ vo init github.com/myuser/myproject
```

Creates `vo.mod`:

```
module github.com/myuser/myproject
vo 0.1
```

### 13.2 `vo get <alias> <module-path>@<version>`

- Downloads the module source to cache (`.vodeps` and/or global cache)
- Ensures `<module>@<version>/vo.mod` exists and module path matches
- Adds (or updates) a direct `require <alias> <module> <version>` line in root `vo.mod`
- Updates `vo.sum`
- Does **not** resolve or upgrade other dependencies

```bash
$ vo get gin github.com/gin-gonic/gin@v1.9.0
```

### 13.3 `vo sync`

- Reads root `vo.mod`
- Ensures full transitive closure is present in cache
- Refreshes/repairs `vo.sum` entries for resolved versions

```bash
$ vo sync
```

### 13.4 `vo build`

- Reads root `vo.mod`
- Computes transitive closure by reading dependency `vo.mod` files
- Enforces single-version-per-module rule
- Resolves imports using the algorithm in §6
- In `--offline` mode: never downloads modules

```bash
$ vo build
$ vo build --offline
```

### 13.5 `vo check`

- Uses the same module-resolution/closure rules as `vo build`
- In `--offline` mode: never downloads modules

## 14. Not Supported (By Design)

| Feature | Status |
|---------|--------|
| Version ranges or constraints | Not supported |
| Automatic version selection / dependency solving | Not supported |
| Multiple versions of the same module in one build | Not supported |
| `replace`/`exclude`/overrides | Not supported |
| Vendor shadowing | Not supported |
| Implicit network access during `vo build --offline` | Not supported |

## 15. Native Extensions

### 15.1 Overview

Native extensions allow a module to provide Rust implementations for extern functions.
An extension is discovered from `vo.ext.toml` in module/package roots and loaded as a dynamic library.

### 15.2 Required Files

For a module with native implementation:

```
<module-root>/
├── vo.mod
├── vo.sum
├── vo.ext.toml
├── <pkg>/*.vo
└── rust/
    ├── Cargo.toml
    └── src/lib.rs
```

Build outputs (`*.so`, `*.dylib`, `*.dll`, `target/`) are not committed.

### 15.3 Manifest Format (`vo.ext.toml`)

`vo.ext.toml` must contain:

```toml
[extension]
name = "mylib"

[native]
path = "rust/target/{profile}/libmylib"
```

Rules:

- `extension.name` must be non-empty.
- `native.path` is relative to manifest directory.
- If extension suffix is omitted, the platform suffix is appended (`.so` / `.dylib` / `.dll`).
- `{profile}` placeholder is allowed.

### 15.4 ABI Compatibility

Native libraries must export `vo_ext_get_entries`.
Runtime validates ABI version. Mismatch is a hard load error.

### 15.5 Platform Layering for Extension Modules

A module that provides native extension implementations must decide how to handle non-native targets.

**Option A — Native-only (WASM unsupported)**

Place `extern func` declarations in a non-guarded or `//go:build native` file. Do not provide a `//go:build wasm` counterpart. WASM builds fail at compile time with a missing-implementation error. This is the correct outcome.

**Option B — Native extension + WASM fallback**

Provide two implementation files:

```
mylib/
├── math.vo              // shared API surface (types, pure functions)
├── math_native.vo       //go:build native  — extern func FastAdd(a, b int) int
├── math_wasm.vo         //go:build wasm    — func FastAdd(a, b int) int { return a + b }
└── vo.ext.toml          // registers Rust dylib for native target only
```

The `vo.ext.toml` is only activated when the ext loader runs (native builds). On WASM, the ext loader does not run, and the `//go:build wasm` file provides the Vo implementation. No manifest or capability declaration is needed — the compiler proves correctness by succeeding.

**There is no third option.** A module cannot claim WASM support without providing WASM-compatible implementations for every `extern func` it exposes.

## 16. Typical Errors

### 16.1 Missing Cached Module

```
error: cannot find module github.com/foo/bar@v1.2.3 in .vodeps
  run: vo get github.com/foo/bar@v1.2.3
```

### 16.2 Version Conflict

```
error: module github.com/foo/bar required at both v1.2.3 and v1.2.4
  v1.2.3 required by: github.com/myuser/myproject
  v1.2.4 required by: github.com/myuser/myproject -> github.com/baz/qux@v0.1.0
```

### 16.3 Unowned Import Path

```
error: import path "github.com/unknown/pkg" is not in std/ and matches no module in the closure
```

### 16.4 Import Cycle

```
error: import cycle detected
  github.com/myuser/myproject/a imports
  github.com/myuser/myproject/b imports
  github.com/myuser/myproject/a
```

### 16.5 Internal Package Violation

```
error: use of internal package not allowed
  github.com/other/project cannot import
  github.com/myuser/myproject/internal/secret
```

### 16.6 Native Extension ABI Mismatch

```
error: failed to load extension mylib
  ABI version mismatch: expected <runtime>, found <extension>
```

## 17. Related Specifications

- `repository-layout.md` (GitHub repository layout and release conventions)
