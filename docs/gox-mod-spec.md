# GoX Module Specification (Transitive Closure Model)

Version: 1.0  
Status: Draft

## 1. Overview

GoX uses a deterministic module system with pinned versions and offline builds.
Dependencies are declared in `gox.mod`, and transitive dependencies are discovered by recursively reading each dependency's own `gox.mod`. There is no version solving: any version disagreement is a hard error.

**Core Properties:**

- Exact versions only (including optional pre-release/build metadata)
- `gox build` never accesses the network
- Transitive dependency closure is computed from `gox.mod` files
- A module path may appear with only one version in the entire closure

## 2. Module Definition

### 2.1 The `gox.mod` File

Every module has a `gox.mod` at its root containing:

- Exactly one `module` line
- Zero or more `require` lines (direct dependencies)

**Format:**

```
module <module-path>

require <module-path> <version>
require <module-path> <version>
...
```

**Example:**

```
module github.com/myuser/myproject

require github.com/foo/bar v1.2.3
require github.com/baz/qux v0.1.0
```

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
├── gox.mod
├── main.gox
├── util/
│   └── helper.gox
└── .goxdeps/
    └── github.com/
        └── foo/
            └── bar@v1.2.3/
                ├── gox.mod
                └── ...
```

**Rules:**

- `.goxdeps/` is a local cache; it is not committed to VCS.
- Each cached dependency module lives at: `.goxdeps/<module-path>@<version>/`
- The cached module root must contain its own `gox.mod`.

## 4. Standard Library

- Standard library packages are imported with the reserved prefix `std/`, e.g.:
  - `import "std/io"`
  - `import "std/strings"`
- The compiler manages the standard library location internally.
- No user or third-party module may define packages under the `std/` prefix.

## 5. Dependency Closure and Version Consistency

### 5.1 Closure Computation

Given a root module R:

1. Read `R/gox.mod`
2. For each `require M V`, load module `M@V` from `.goxdeps/M@V/gox.mod`
3. Recursively repeat for each loaded module's `require` lines
4. Maintain a visited set to avoid infinite recursion

This produces a **module closure**: a set of `(module-path -> version)` pairs.

### 5.2 Version Conflict Rule (No Solver)

A module path may appear at **only one version** across the entire closure.

- If the closure requires both `github.com/foo/bar v1.2.3` and `github.com/foo/bar v1.2.4`, that is a **hard error**.
- The toolchain must report the conflict and show at least one dependency chain for each side of the conflict.

### 5.3 Offline Build Rule

`gox build` must not download anything.

- If any required module directory is missing from `.goxdeps`, the build fails.
- The error should indicate the missing `(module, version)` and suggest `gox get`.

## 6. Imports and Resolution

### 6.1 Import Syntax

GoX supports the following import forms:

```go
import "path/to/package"
import alias "path/to/package"
import . "path/to/package"      // Dot import (import all exported names)
import _ "path/to/package"      // Blank import (init only)
```

### 6.2 Package Paths vs Module Paths

- `require` declares **modules**.
- `import` targets **packages** inside modules.

A package import path is resolved by first identifying which module owns it, then mapping the remainder to a directory inside that module.

### 6.3 Resolution Algorithm

For an import path `P`:

1. **Standard library**
   - If `P` starts with `std/`:
     - Resolve to the compiler's internal standard library location
     - If not found: error

2. **Owning module by longest prefix match**
   - Let `S` be the set of module paths in the closure plus the root module path.
   - Find the longest module path `M` in `S` such that `P == M` or `P` starts with `M + "/"`.
   - If none found: error (missing dependency or missing `require`)

3. **Map to filesystem**
   - If `M` is the root module:
     - The package directory is `<project-root>/<rest>/`
   - If `M` is a dependency module at version `V`:
     - The package directory is `.goxdeps/<M>@<V>/<rest>/`
   - Where `<rest>` is:
     - empty if `P == M` (package at module root), otherwise the path suffix after `M/`.

**Example** (root module `github.com/myuser/myproject` and `require github.com/foo/bar v1.2.3`):

| Import Path | Resolution |
|-------------|------------|
| `"std/io"` | Standard library `io` package |
| `"github.com/myuser/myproject/util"` | `./util/` |
| `"github.com/foo/bar"` | `.goxdeps/github.com/foo/bar@v1.2.3/` |
| `"github.com/foo/bar/sub"` | `.goxdeps/github.com/foo/bar@v1.2.3/sub/` |

## 7. Package Rules

These rules define how directories form packages; they are required for deterministic builds.

1. A directory corresponds to a single package.
2. All `.gox` files in the same directory must declare the same `package <name>`.
3. Package name matching the directory name is **recommended** but not required.
4. Multiple files in a directory are compiled together as one package.
5. Files are processed in **alphabetical order** by filename.

### 7.1 The `main` Package

- A package named `main` with a `func main()` is an executable entry point.
- The `main` package cannot be imported by other packages.

### 7.2 Test Files

Files with the `_test.gox` suffix are only compiled during testing:

```
util/
├── helper.gox       # Always compiled
└── helper_test.gox  # Only compiled for tests
```

Test files may declare either:

- `package util` - Internal tests (access to unexported symbols)
- `package util_test` - External tests (black-box testing)

## 8. Internal Packages

GoX supports Go-style internal visibility:

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

Or the legacy syntax:

```go
// +build linux

package mypackage
```

Files are included only when the build tags match.

## 13. CLI Commands

### 13.1 `gox init <module-path>`

Creates a new `gox.mod` in the current directory:

```bash
$ gox init github.com/myuser/myproject
```

Creates `gox.mod`:

```
module github.com/myuser/myproject
```

### 13.2 `gox get <module-path>@<version>`

- Downloads the module source to `.goxdeps/<module>@<version>/`
- Ensures `.goxdeps/<module>@<version>/gox.mod` exists
- Adds (or updates) a direct `require` line in the root `gox.mod`
- Does **not** resolve or upgrade other dependencies

```bash
$ gox get github.com/foo/bar@v1.2.3
```

### 13.3 `gox build`

- Reads root `gox.mod`
- Computes transitive closure by reading dependency `gox.mod` files
- Enforces single-version-per-module rule
- Resolves imports using the algorithm in §6
- **Never downloads modules** during the build

```bash
$ gox build
```

## 14. Not Supported (By Design)

| Feature | Status |
|---------|--------|
| Version ranges or constraints | Not supported |
| Automatic version selection / dependency solving | Not supported |
| Multiple versions of the same module in one build | Not supported |
| `replace`/`exclude`/overrides | Not supported |
| Vendor shadowing | Not supported |
| Implicit network access during `gox build` | Not supported |

## 15. Typical Errors

### Missing Cached Module

```
error: cannot find module github.com/foo/bar@v1.2.3 in .goxdeps
  run: gox get github.com/foo/bar@v1.2.3
```

### Version Conflict

```
error: module github.com/foo/bar required at both v1.2.3 and v1.2.4
  v1.2.3 required by: github.com/myuser/myproject
  v1.2.4 required by: github.com/myuser/myproject -> github.com/baz/qux@v0.1.0
```

### Unowned Import Path

```
error: import path "github.com/unknown/pkg" is not in std/ and matches no module in the closure
```

### Import Cycle

```
error: import cycle detected
  github.com/myuser/myproject/a imports
  github.com/myuser/myproject/b imports
  github.com/myuser/myproject/a
```

### Internal Package Violation

```
error: use of internal package not allowed
  github.com/other/project cannot import
  github.com/myuser/myproject/internal/secret
```
