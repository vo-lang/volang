# Vo Module Specification

Version: 1.0  
Status: Draft

## 1. Scope

This document defines module identity, package import rules, dependency declaration, dependency resolution, lock files, workspace overrides, registry behavior, integrity checks, and build/network policy for Vo.

This specification is complementary to:

- `repository-layout.md`
- `native-ffi.md`

## 2. Design Principles

The Vo module system is defined by the following principles:

- **Canonical identity**. Every published module and package has exactly one canonical import path.
- **Deterministic builds**. A build uses an already-resolved dependency graph and does not silently change it.
- **Separation of intent and resolution**. `vo.mod` expresses human-authored dependency intent; `vo.lock` records the exact resolved graph.
- **Frozen build commands**. `vo build`, `vo check`, `vo test`, and `vo run` do not access the network.
- **Target-neutral dependency graph**. Native, WASM, and pure-Vo builds use the same module graph.
- **Local development isolation**. `vo.work` affects only local workspace builds and must not change published module identity.
- **GitHub-backed distribution**. The registry for published modules is GitHub Releases.

## 3. Module Identity

### 3.1 Canonical Module Path

A Vo module path is a canonical GitHub path:

```text
github.com/<owner>/<repo>[/<subdir>][/vN]
```

Rules:

- The path must begin with `github.com/`.
- `<owner>` and `<repo>` identify the backing GitHub repository.
- `/<subdir>` is allowed for modules published from a subdirectory of a repository.
- `/vN` is the major-version suffix for incompatible major versions where `N >= 2`.
- The canonical module path is the module's permanent identity.
- Two modules are the same module only if their canonical module paths are identical.

Examples:

```text
github.com/vo-lang/vogui
github.com/vo-lang/mono/graphics
github.com/vo-lang/voplay/v2
```

### 3.2 Major Version Rule

For major version `v0` and `v1`, the module path has no major suffix.
For major version `v2` and above, the canonical module path must end in `/vN`.

Examples:

- `github.com/acme/lib` may publish `v0.x.y` and `v1.x.y`
- `github.com/acme/lib/v2` publishes `v2.x.y`
- `github.com/acme/lib` and `github.com/acme/lib/v2` are distinct modules

### 3.3 Package Identity

A package is identified by its full canonical import path.
A package path is either:

- `std/<name-or-subpath>` for standard-library packages
- `<module-path>` for the module root package
- `<module-path>/<package-subpath>` for a package inside a module

Examples:

```text
std/fmt
github.com/acme/app
github.com/acme/app/internal/cache
github.com/vo-lang/vogui/widget
```

## 4. Imports

### 4.1 Import Forms

Vo supports the following import forms:

```go
import "std/fmt"
import "github.com/acme/app/util"
import util "github.com/acme/app/util"
import . "github.com/acme/app/testing"
import _ "github.com/acme/app/metrics"
```

Rules:

- `std/` is reserved for the standard library.
- All non-stdlib imports must use canonical package paths.
- Import aliases are lexical only; they do not change module identity.
- The `@` marker is not part of the module system.
- Relative imports such as `"./util"` and `"../x"` are not allowed.
- Bare project-root imports such as `"util"` are not allowed unless `util` is a standard-library package under `std/util`.

### 4.2 Intra-Module Imports

Packages inside the same module are imported by full canonical path, not by project-relative path.

Example:

```go
module github.com/acme/app
```

```go
import "github.com/acme/app/util"
import "github.com/acme/app/http/server"
```

This rule ensures that package identity is uniform in source code, lock files, caches, tooling, and diagnostics.

### 4.3 Standard Library Imports

Standard-library packages use the reserved `std/` prefix.
No third-party module may define packages under `std/`.

Examples:

```go
import "std/fmt"
import "std/io"
import "std/strings"
```

## 5. Module Files

### 5.1 `vo.mod`

Every published module must contain a `vo.mod` file at its module root.

`vo.mod` is the human-authored manifest. It defines:

- the module's canonical path
- the supported Vo toolchain constraint
- the module's direct dependency constraints

`vo.mod` does not contain:

- transitive dependency resolution results
- local workspace overrides
- checksums for the resolved graph
- release artifact listings

Format:

```text
module <module-path>
vo <toolchain-constraint>

require <module-path> <version-constraint>
require <module-path> <version-constraint>
...
```

Example:

```text
module github.com/acme/app
vo ^1.0.0

require github.com/vo-lang/vogui ^0.4.0
require github.com/vo-lang/voplay ~0.7.2
require github.com/acme/http v1.3.1
```

Rules:

- There must be exactly one `module` line.
- There must be exactly one `vo` line.
- `require` entries declare direct dependencies only.
- A given module path may appear at most once in the `require` list.
- A dependency is identified only by canonical module path, never by alias.

### 5.2 Version Constraints in `vo.mod`

A `require` line uses one of the following constraint forms:

- **Exact**: `v1.2.3`
- **Compatible**: `^1.2.3` or `^0.4.0`
- **Patch-compatible**: `~1.2.3`

Normative meaning:

- `v1.2.3` means exactly `v1.2.3`
- `^1.2.3` means `>= v1.2.3` and `< v2.0.0`
- `^0.4.0` means `>= v0.4.0` and `< v0.5.0`
- `~1.2.3` means `>= v1.2.3` and `< v1.3.0`

Pre-release versions satisfy a range only when the range explicitly names a pre-release version.

### 5.3 `vo.lock`

`vo.lock` is the machine-generated lock file.
It records the exact resolved dependency graph used by build commands.

`vo.lock` must contain, at minimum, for every resolved module:

- canonical module path
- exact resolved version
- immutable source revision identity
- verified source artifact digest
- verified release-manifest digest
- any locked target-specific artifact digests selected for the project

A `vo.lock` file is authoritative for builds.
If `vo.mod` is human intent, `vo.lock` is the exact executable contract.

An illustrative TOML shape is:

```toml
version = 1
created_by = "vo 1.0.0"

[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[module]]
path = "github.com/vo-lang/vogui"
version = "v0.4.2"
commit = "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22"
release_manifest = "sha256:2f7d..."
source = "sha256:81c1..."

[[module.artifact]]
kind = "extension-wasm"
target = "wasm32-unknown-unknown"
name = "vogui-wasm.wasm"
digest = "sha256:6f92..."
```

The exact serialization format of `vo.lock` is part of the toolchain contract and must be stable across compatible toolchain releases.

### 5.4 `vo.work`

`vo.work` is the local workspace override file.
It is not part of published module semantics.

Purpose:

- map canonical module paths to local source directories during development
- enable multi-module local development without changing published dependency identity

Illustrative format:

```text
use github.com/vo-lang/vogui => ../vogui
use github.com/vo-lang/voplay => ../voplay
```

Rules:

- A workspace override is keyed by canonical module path.
- The target directory must contain a `vo.mod` whose `module` line matches that canonical path.
- `vo.work` affects only the local workspace where it is used.
- Published modules must not rely on consumers reading the publisher's `vo.work`.
- Tooling must provide a way to ignore `vo.work` for release and CI builds.

### 5.5 `vo.ext.toml`

A module may contain `vo.ext.toml` to declare extension metadata.
That file is part of the source package and versioned with the module.
Its semantics are defined in `native-ffi.md`.

The module system treats extension metadata as part of a module's published contents, not as a separate dependency system.

## 6. GitHub Registry Model

### 6.1 Registry Source

The Vo module registry is GitHub Releases.
A published module version is resolved from GitHub by canonical module path and version.

Each published module version must map to:

- an immutable Git tag
- a GitHub Release for that tag
- a release manifest asset
- a source-package asset

### 6.2 Release Manifest

Each release must provide a machine-readable release manifest asset named `vo.release.json`.
The release manifest must contain, at minimum:

- `module`: canonical module path
- `version`: exact module version
- `commit`: immutable Git revision
- `vo`: toolchain constraint from `vo.mod`
- `require`: direct dependency constraints from `vo.mod`
- `source`: source-package asset name, size, and digest
- `artifacts`: optional target-specific artifact metadata and digests

The release manifest is registry metadata.
It allows dependency resolution and integrity verification without interpreting arbitrary repository layout as package protocol.

### 6.3 Source Package

Each release must provide a canonical source-package asset.
The source package is the authoritative published source for the module version.

Rules:

- The source package must unpack to the module root.
- The source package must include `vo.mod`.
- If present in the module, it must include `vo.lock` and `vo.ext.toml`.
- The source package must include all `.vo` files required to build the module.
- The source package digest must match both the release manifest and `vo.lock`.

### 6.4 Target-Specific Artifacts

A release may provide target-specific binary artifacts.
Examples include:

- prebuilt native extension libraries
- WASM extension binaries
- generated bridge files required by a target runtime

Rules:

- Target-specific artifacts must be listed in `vo.release.json`.
- Binary artifacts must be verified by digest before use.
- Binary artifacts do not change module identity or dependency resolution.
- A module remains source-defined even when binary artifacts are present.

## 7. Dependency Resolution

### 7.1 Resolution Inputs

Resolution takes the following inputs:

- the root module's `vo.mod`
- optional local overrides from `vo.work`
- registry metadata from `vo.release.json`
- the set of versions available in GitHub Releases for each required module path

### 7.2 Resolution Output

Resolution produces a complete module graph with:

- exactly one resolved version per canonical module path
- verified registry metadata for each module
- exact source artifact digests
- exact optional target-artifact digests when the project locks them

The output is written to `vo.lock`.

### 7.3 Single-Version Rule

Within one resolved build graph, a canonical module path may appear at only one exact version.

Examples:

- `github.com/acme/lib v1.2.3` and `github.com/acme/lib v1.4.0` in the same graph is an error
- `github.com/acme/lib` and `github.com/acme/lib/v2` may both appear because they are distinct module paths

### 7.4 Deterministic Solver Behavior

When multiple versions satisfy all constraints, the solver must select a single deterministic answer.
The normative policy is:

1. Prefer the highest semantic version that satisfies all active constraints.
2. Never cross a major-path boundary.
3. Break no ties by network order, release timestamp order, or cache order.

If no version satisfies all constraints, resolution fails.

### 7.5 When Resolution Happens

Dependency resolution is performed by explicit module-management commands.
Build commands do not silently re-resolve the graph.

The toolchain must provide commands equivalent in behavior to:

- `vo mod add`
- `vo mod update`
- `vo mod sync`
- `vo mod download`

These commands may access the network.

## 8. Build and Network Policy

### 8.1 Frozen Build Commands

The following commands are frozen with respect to dependency resolution:

- `vo build`
- `vo check`
- `vo test`
- `vo run`

Frozen means:

- no registry access
- no implicit version solving
- no lockfile mutation
- no silent dependency upgrades

### 8.2 Missing Lock or Missing Artifacts

If a build requires external modules and `vo.lock` is missing, the build fails and instructs the user to run a module-management command.

If `vo.lock` exists but required artifacts are missing from cache, the build fails and instructs the user to fetch them explicitly.

### 8.3 Cache Model

The cache is an implementation detail.
It is not the source of truth for dependency identity.

Rules:

- The source of truth for builds is `vo.lock`.
- Cache entries should be content-addressed by verified digest.
- A cache hit is valid only when the digest matches the locked digest.
- The toolchain may use a global cache, a project-local cache, or both.
- Cache layout is not part of the module identity contract.

## 9. Package Ownership and Import Resolution

### 9.1 Resolution Algorithm

For an import path `P`:

1. If `P` begins with `std/`, resolve it as a standard-library package.
2. Otherwise, find the resolved module whose canonical module path is the longest prefix of `P`.
3. The remainder of `P` after that prefix is the package subpath inside the module.
4. Resolve that directory inside the selected module's verified source tree.
5. If no resolved module owns `P`, the import is an error.

Examples:

- `std/fmt` resolves to the standard library
- `github.com/acme/app/util` resolves to package `util` inside module `github.com/acme/app`
- `github.com/vo-lang/vogui/widget` resolves inside module `github.com/vo-lang/vogui`

### 9.2 Root Packages

The module root directory is the package whose import path equals the module path.

Example:

- module path: `github.com/acme/app`
- root package import path: `github.com/acme/app`

### 9.3 Package Rules

Rules:

- One directory defines one package.
- All `.vo` files in one directory must declare the same package name.
- Multiple files in one package are compiled together.
- Files with `_test.vo` suffix are included only in test builds.
- A package named `main` is an executable package and cannot be imported.

### 9.4 Internal Packages

Vo supports Go-style internal visibility.

Rule:

- If an import path contains `/internal/`, that package may be imported only by packages whose canonical import path shares the parent prefix before `/internal/`.

Example:

- internal package: `github.com/acme/app/internal/cache`
- allowed importer: `github.com/acme/app/cmd/tool`
- disallowed importer: `github.com/other/project/tool`

## 10. Projects Without `vo.mod`

A single-file or single-package program without `vo.mod` is an ad hoc program, not a published module.

Rules:

- It may import only `std/...` packages.
- It may not declare external module dependencies.
- It may not import sibling directories as packages.
- Any multi-package project or project with external dependencies must define `vo.mod`.

## 11. Workspace Overrides

### 11.1 Override Semantics

When `vo.work` is active, the toolchain may replace a resolved module's registry source tree with a local directory for development.

Rules:

- The local directory must identify the same canonical module path.
- The override does not rename the module.
- Import paths remain canonical.
- The override is local state and must not be assumed by downstream consumers.

### 11.2 Interaction with Lock Files

A workspace-aware toolchain may use local override metadata when refreshing a lock file for local development.
However:

- a published module's committed `vo.lock` must be generated with workspace overrides disabled
- CI and release workflows must provide a mode that ignores `vo.work`

This keeps local development convenient without leaking local paths into published module semantics.

## 12. Integrity Rules

### 12.1 Verification Requirements

The toolchain must verify:

- release-manifest digest
- source-package digest
- any used target-artifact digest
- module path and version consistency across `vo.mod`, `vo.lock`, Git tag, and release manifest

Any mismatch is a hard error.

### 12.2 No Implicit Trust in Repository Trees

The module protocol is based on release metadata and verified artifacts.
A toolchain must not treat arbitrary repository trees, raw file URLs, or archive snapshots outside the declared release protocol as equivalent to a published module release.

## 13. CLI Behavior

The toolchain must expose module-management behavior equivalent to the following commands.
The exact command spelling may differ, but the semantics are normative.

### 13.1 `vo mod init <module-path>`

Creates a new `vo.mod` for the current module.

### 13.2 `vo mod add <module-path>[@constraint]`

Adds or updates a direct dependency constraint in `vo.mod`.
This command may refresh `vo.lock`.

### 13.3 `vo mod update [module-path]`

Re-solves dependency constraints, usually selecting newer compatible versions.
Updates `vo.lock`.

### 13.4 `vo mod sync`

Recomputes the full dependency graph from `vo.mod` and writes a fresh `vo.lock`.
It verifies registry metadata and selected artifacts.

### 13.5 `vo mod download`

Fetches artifacts already pinned by `vo.lock` into cache without changing the resolved graph.

### 13.6 `vo build`, `vo check`, `vo test`, `vo run`

Use the exact graph pinned in `vo.lock`.
Do not access the network.
Do not mutate `vo.mod` or `vo.lock`.

## 14. Not Supported by Design

The following are not part of the Vo module system:

- dependency aliases such as `@"name"`
- project-relative package imports
- published `replace` directives inside `vo.mod`
- multiple exact versions of the same canonical module path in one build graph
- implicit registry access during build commands
- interpreting arbitrary GitHub repository layout as the package protocol

## 15. Typical Errors

### 15.1 Missing Lock File

```text
error: this build requires external modules but vo.lock is missing
  run: vo mod sync
```

### 15.2 Unsatisfied Constraints

```text
error: no version of github.com/acme/http satisfies all constraints
  root requires: ^1.4.0
  github.com/acme/appkit requires: ~1.3.2
```

### 15.3 Unknown Import Path

```text
error: import path "github.com/acme/unknown/pkg" is not owned by std/ or any resolved module
```

### 15.4 Integrity Mismatch

```text
error: digest mismatch for github.com/acme/lib v1.2.3
  expected source digest: sha256:81c1...
  actual source digest:   sha256:19ae...
```

### 15.5 Internal Package Violation

```text
error: use of internal package not allowed
  github.com/other/project/tool cannot import
  github.com/acme/app/internal/cache
```

### 15.6 Workspace Identity Mismatch

```text
error: workspace override for github.com/vo-lang/vogui points to ../vogui-dev
  expected module path: github.com/vo-lang/vogui
  found in vo.mod:     github.com/acme/forked-vogui
```

## 16. Related Specifications

- `repository-layout.md`
- `native-ffi.md`
