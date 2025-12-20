# GoX Source File Reading and Package Import System Design

## 1. Current State Analysis

### 1.1 Two VFS Systems

Currently there are two independent VFS implementations:

| Location | Core Types | Purpose |
|----------|-----------|---------|
| `gox-common/src/vfs.rs` | `FileSystem` trait, `RealFs`, `MemoryFs`, `FileSet` | Low-level filesystem abstraction |
| `gox-module/src/vfs.rs` | `PackageSource` trait, `StdVfs`, `LocalVfs`, `ModVfs`, `Vfs` | Package path resolution |

**Problems:**
- `gox-module` directly uses `std::fs`, not reusing `gox-common::FileSystem`
- Overlapping functionality but not unified

### 1.2 gox-analysis FileSystem Generic is Unused

```rust
// gox-analysis/src/check/checker.rs
pub struct Checker<F: FileSystem> {
    _phantom: PhantomData<F>,  // Only PhantomData, never actually used
}

pub struct FilesContext<'a, F: FileSystem> {
    _phantom: PhantomData<F>,
}
```

**Impact:** 17 files have `F: FileSystem` generic parameter, but none actually use it to read files.

### 1.3 Package Import Flow is Incomplete

`import_package()` in `gox-analysis/src/check/resolver.rs` is a stub:

```rust
fn import_package(&mut self, path: &str, span: Span) -> PackageKey {
    // Only creates empty package, doesn't actually load source files!
    let pkg = self.tc_objs.new_package(path.to_string());
    // ...
}
```

### 1.4 Missing Key Interfaces

Interfaces expected by `gox-codegen-vm` but not provided by `gox-analysis`:

- `gox_analysis::project::Project` - Multi-package project structure
- `gox_analysis::analyze_project()` - Entry function
- `gox_analysis::types::Type`, `BasicType` - Type definitions
- `gox_analysis::scope::Entity`, `VarEntity`, `FuncEntity` - Scope entities
- `gox_analysis::type_interner::TypeInterner` - Type storage

---

## 2. Design Proposal

### 2.1 VFS Unification

**Keep layers separate, eliminate duplication:**

```
┌─────────────────────────────────────────────────┐
│              gox-module/vfs.rs                  │
│  ┌─────────────────────────────────────────┐    │
│  │  Vfs (Package resolution layer)         │    │
│  │    ├── StdVfs                           │    │
│  │    ├── LocalVfs                         │    │
│  │    └── ModVfs                           │    │
│  │         │                               │    │
│  │         ▼ depends on                    │    │
│  │    FileSystem trait (from gox-common)   │    │
│  └─────────────────────────────────────────┘    │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│              gox-common/vfs.rs                  │
│  FileSystem trait + RealFs + MemoryFs          │
│  FileSet (collect .gox files)                  │
└─────────────────────────────────────────────────┘
```

**Changes:**
- `gox-module`'s `StdVfs`, `LocalVfs`, `ModVfs` internally use `gox-common::FileSystem`
- Tests can use `MemoryFs` to construct virtual filesystems

### 2.2 Package Import Design

**Core idea:** Checker doesn't read files directly; use `Importer` callback to let the caller handle it.

```
┌─────────────────────────────────────────────────────────────────┐
│                     Caller Layer (gox-cli)                      │
│  1. FileSet::collect() gathers initial files                    │
│  2. Create ProjectImporter (implements Importer trait)          │
│  3. Call analyze_project(file_set, importer)                    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     gox-analysis                                │
│  When Checker encounters import "foo/bar":                      │
│    → Call importer.import("foo/bar")                            │
│    → Importer returns PackageKey (resolved package)             │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│              ProjectImporter (implemented in caller)            │
│  fn import(&mut self, key: &ImportKey) -> ImportResult {        │
│      // 1. Use Vfs to resolve path                              │
│      let pkg = self.vfs.resolve(&key.path)?;                    │
│      // 2. Parse source files (parser)                          │
│      let files = parse_files(&pkg.files);                       │
│      // 3. Recursively type check                               │
│      let pkg_key = self.check_package(files)?;                  │
│      Ok(pkg_key)                                                │
│  }                                                              │
└─────────────────────────────────────────────────────────────────┘
```

**Separation of Concerns:**

| Component | Location | Responsibility |
|-----------|----------|----------------|
| `FileSystem` trait | `gox-common` | Low-level file read abstraction |
| `Vfs` | `gox-module` | import path → filesystem path |
| `Importer` trait | `gox-analysis` | Package import callback interface |
| `ProjectImporter` | Caller layer | Combine Vfs + Parser + Checker |

### 2.3 gox-analysis Changes

**Remove unused FileSystem generic:**

```rust
// Before
pub struct Checker<F: FileSystem> {
    _phantom: PhantomData<F>,
}

// After
pub struct Checker {
    // No FileSystem generic needed
}
```

**Checker receives Importer:**

```rust
impl Checker {
    pub fn check_with_importer(
        &mut self, 
        files: &[File], 
        importer: &mut dyn Importer
    ) -> Result<PackageKey, Error> {
        // Call importer.import() when encountering import statements
    }
}
```

---

## 3. Import Path Resolution Rules

### 3.1 Import Syntax

GoX uses `@` symbol to distinguish external dependencies:

```go
import (
    // Standard library (no marker)
    "fmt"
    "strings"
    
    // Local project packages (no marker, relative to project root)
    "utils"
    "handlers/api"
    
    // External dependencies (@ marker, must be declared in gox.mod)
    @"gin"
    @"jwt"
)
```

**Supported import forms:**

| Form | Description |
|------|-------------|
| `import "path"` | Standard library or local package |
| `import @"alias"` | External dependency (defined in gox.mod) |
| `import name "path"` | With alias |
| `import name @"alias"` | External dependency with alias |
| `import . "path"` | Dot import |
| `import _ "path"` | Blank import (init only) |

### 3.2 gox.mod Format

```
module myproject

require gin github.com/gin-gonic/gin v1.9.0
require jwt github.com/golang-jwt/jwt v5.0.0
```

Format: `require <alias> <module-path> <version>`

- **alias**: Name used in source code with `@"xxx"`
- **module-path**: Actual repository URL
- **version**: Exact version number

### 3.3 Resolution Algorithm

For import path `P`:

| Step | Condition | Action |
|------|-----------|--------|
| 1 | `@"P"` form | Look up alias `P` in gox.mod → map to `.goxdeps/<module-path>@<version>/` |
| 2 | `"P"` and P is known stdlib | Resolve to compiler's internal stdlib location |
| 3 | `"P"` and P is not stdlib | Look for `<project-root>/<P>/` directory |

### 3.4 VFS Layer Mapping

| VFS Layer | Condition | Example |
|-----------|-----------|---------|
| (No VFS - alias lookup) | `@"xxx"` form | `@"gin"` → gox.mod alias → `.goxdeps/...` |
| `StdVfs` | Known stdlib name | `"fmt"`, `"strings"` |
| `LocalVfs` | Non-stdlib regular path | `"utils"`, `"handlers/api"` |

---

## 4. Implementation Steps

### Phase 1: Simplify gox-analysis

1. Remove `FileSystem` generic from `Checker<F>` and `FilesContext<F>`
2. Affects 17 files' `use gox_common::vfs::FileSystem` and generic parameters
3. Keep `Importer` trait unchanged

### Phase 2: Unify VFS

1. `gox-module`'s `StdVfs`, `LocalVfs`, `ModVfs` depend on `gox-common::FileSystem`
2. Add generic parameter `<F: FileSystem = RealFs>`
3. Tests can use `MemoryFs`

### Phase 3: Implement Real Package Import

1. Implement `ProjectImporter` in caller layer (`gox-cli` or new `gox-driver`)
2. Implement `analyze_project()` function
3. Connect `ModuleResolver` and `Checker`

---

## 5. Dependency Graph

```
gox-common
├── vfs.rs (FileSystem, RealFs, MemoryFs, FileSet)
└── source.rs (SourceMap, SourceFile)
      │
      ▼
gox-module
├── modfile.rs (ModFile, Require)
├── resolver.rs (ModuleResolver, ModuleClosure)
└── vfs.rs (Vfs, StdVfs, LocalVfs, ModVfs) ← depends on gox-common::FileSystem
      │
      ▼
gox-analysis
├── importer.rs (Importer trait)
├── check/checker.rs (Checker) ← receives &mut dyn Importer
└── ... (type checking logic)
      │
      ▼
Caller Layer (gox-cli / gox-driver)
└── ProjectImporter (implements Importer)
    └── Combines Vfs + Parser + Checker
```

---

## 6. File Changes Summary

### Files to Modify in gox-analysis (remove FileSystem generic)

- `check/checker.rs` - `Checker<F>` → `Checker`
- `check/assignment.rs`
- `check/builtin.rs`
- `check/call.rs`
- `check/conversion.rs`
- `check/decl.rs`
- `check/expr.rs`
- `check/initorder.rs`
- `check/interface.rs`
- `check/label.rs`
- `check/resolver.rs`
- `check/returns.rs`
- `check/stmt.rs`
- `check/typexpr.rs`
- `check/util.rs`
- `lookup.rs`
- `operand.rs`

### Files to Modify in gox-module

- `vfs.rs` - Add `FileSystem` generic parameter to `StdVfs`, `LocalVfs`, `ModVfs`

### New Files to Create

- `gox-analysis/src/project.rs` - `Project`, `PackageInfo` structures
- Caller layer: `ProjectImporter` implementation
