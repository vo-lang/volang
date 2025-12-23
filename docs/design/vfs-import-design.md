# Vo Source File Reading and Package Import System Design

## 1. Current State Analysis

### 1.1 Two VFS Systems

Currently there are two independent VFS implementations:

| Location | Core Types | Purpose |
|----------|-----------|---------|
| `vo-common/src/vfs.rs` | `FileSystem` trait, `RealFs`, `MemoryFs`, `FileSet` | Low-level filesystem abstraction |
| `vo-module/src/vfs.rs` | `PackageSource` trait, `StdVfs`, `LocalVfs`, `ModVfs`, `Vfs` | Package path resolution |

**Problems:**
- `vo-module` directly uses `std::fs`, not reusing `vo-common::FileSystem`
- Overlapping functionality but not unified

### 1.2 vo-analysis FileSystem Generic is Unused

```rust
// vo-analysis/src/check/checker.rs
pub struct Checker<F: FileSystem> {
    _phantom: PhantomData<F>,  // Only PhantomData, never actually used
}

pub struct FilesContext<'a, F: FileSystem> {
    _phantom: PhantomData<F>,
}
```

**Impact:** 17 files have `F: FileSystem` generic parameter, but none actually use it to read files.

### 1.3 Package Import Flow is Incomplete

`import_package()` in `vo-analysis/src/check/resolver.rs` is a stub:

```rust
fn import_package(&mut self, path: &str, span: Span) -> PackageKey {
    // Only creates empty package, doesn't actually load source files!
    let pkg = self.tc_objs.new_package(path.to_string());
    // ...
}
```

### 1.4 Missing Key Interfaces

Interfaces expected by `vo-codegen-vm` but not provided by `vo-analysis`:

- `vo_analysis::project::Project` - Multi-package project structure
- `vo_analysis::analyze_project()` - Entry function
- `vo_analysis::types::Type`, `BasicType` - Type definitions
- `vo_analysis::scope::Entity`, `VarEntity`, `FuncEntity` - Scope entities
- `vo_analysis::type_interner::TypeInterner` - Type storage

---

## 2. Design Proposal

### 2.1 VFS Unification

**Keep layers separate, eliminate duplication:**

```
┌─────────────────────────────────────────────────┐
│              vo-module/vfs.rs                  │
│  ┌─────────────────────────────────────────┐    │
│  │  Vfs (Package resolution layer)         │    │
│  │    ├── StdVfs                           │    │
│  │    ├── LocalVfs                         │    │
│  │    └── ModVfs                           │    │
│  │         │                               │    │
│  │         ▼ depends on                    │    │
│  │    FileSystem trait (from vo-common)   │    │
│  └─────────────────────────────────────────┘    │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│              vo-common/vfs.rs                  │
│  FileSystem trait + RealFs + MemoryFs          │
│  FileSet (collect .vo files)                  │
└─────────────────────────────────────────────────┘
```

**Changes:**
- `vo-module`'s `StdVfs`, `LocalVfs`, `ModVfs` internally use `vo-common::FileSystem`
- Tests can use `MemoryFs` to construct virtual filesystems

### 2.2 Package Import Design

**Core idea:** Checker doesn't read files directly; use `Importer` callback to let the caller handle it.

```
┌─────────────────────────────────────────────────────────────────┐
│                     Caller Layer (vo-cli)                      │
│  1. FileSet::collect() gathers initial files                    │
│  2. Create ProjectImporter (implements Importer trait)          │
│  3. Call analyze_project(file_set, importer)                    │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     vo-analysis                                │
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
| `FileSystem` trait | `vo-common` | Low-level file read abstraction |
| `Vfs` | `vo-module` | import path → filesystem path |
| `Importer` trait | `vo-analysis` | Package import callback interface |
| `ProjectImporter` | Caller layer | Combine Vfs + Parser + Checker |

### 2.3 vo-analysis Changes

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

Vo uses `@` symbol to distinguish external dependencies:

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

| Form | Description |
|------|-------------|
| `import "path"` | Standard library or local package |
| `import @"alias"` | External dependency (defined in vo.mod) |
| `import name "path"` | With alias |
| `import name @"alias"` | External dependency with alias |
| `import . "path"` | Dot import |
| `import _ "path"` | Blank import (init only) |

### 3.2 vo.mod Format

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
| 1 | `@"P"` form | Look up alias `P` in vo.mod → map to `.vodeps/<module-path>@<version>/` |
| 2 | `"P"` and P is known stdlib | Resolve to compiler's internal stdlib location |
| 3 | `"P"` and P is not stdlib | Look for `<project-root>/<P>/` directory |

### 3.4 VFS Layer Mapping

| VFS Layer | Condition | Example |
|-----------|-----------|---------|
| (No VFS - alias lookup) | `@"xxx"` form | `@"gin"` → vo.mod alias → `.vodeps/...` |
| `StdVfs` | Known stdlib name | `"fmt"`, `"strings"` |
| `LocalVfs` | Non-stdlib regular path | `"utils"`, `"handlers/api"` |

---

## 4. Implementation Steps

### Phase 1: Simplify vo-analysis

1. Remove `FileSystem` generic from `Checker<F>` and `FilesContext<F>`
2. Affects 17 files' `use vo_common::vfs::FileSystem` and generic parameters
3. Keep `Importer` trait unchanged

### Phase 2: Unify VFS

1. `vo-module`'s `StdVfs`, `LocalVfs`, `ModVfs` depend on `vo-common::FileSystem`
2. Add generic parameter `<F: FileSystem = RealFs>`
3. Tests can use `MemoryFs`

### Phase 3: Implement Real Package Import

1. Implement `ProjectImporter` in caller layer (`vo-cli` or new `vo-driver`)
2. Implement `analyze_project()` function
3. Connect `ModuleResolver` and `Checker`

---

## 5. Dependency Graph

```
vo-common
├── vfs.rs (FileSystem, RealFs, MemoryFs, FileSet)
└── source.rs (SourceMap, SourceFile)
      │
      ▼
vo-module
├── modfile.rs (ModFile, Require)
├── resolver.rs (ModuleResolver, ModuleClosure)
└── vfs.rs (Vfs, StdVfs, LocalVfs, ModVfs) ← depends on vo-common::FileSystem
      │
      ▼
vo-analysis
├── importer.rs (Importer trait)
├── check/checker.rs (Checker) ← receives &mut dyn Importer
└── ... (type checking logic)
      │
      ▼
Caller Layer (vo-cli / vo-driver)
└── ProjectImporter (implements Importer)
    └── Combines Vfs + Parser + Checker
```

---

## 6. File Changes Summary

### Files to Modify in vo-analysis (remove FileSystem generic)

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

### Files to Modify in vo-module

- `vfs.rs` - Add `FileSystem` generic parameter to `StdVfs`, `LocalVfs`, `ModVfs`

### New Files to Create

- `vo-analysis/src/project.rs` - `Project`, `PackageInfo` structures
- Caller layer: `ProjectImporter` implementation
