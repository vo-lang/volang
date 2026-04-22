# Vo Module Specification

Version: 1.0  
Status: Draft

## 1. Scope

This specification defines the normative Vo module protocol. It fixes the behavior of:

- canonical module and package identity
- import-path classification and ownership
- semantic-version syntax and constraint semantics
- `vo.mod`, `vo.lock`, `vo.work`, and `vo.release.json`
- GitHub-backed publication, discovery, and integrity verification
- dependency resolution and lockfile generation
- frozen-build materialization, cache validation, and workspace overrides
- normative CLI behavior for module-management commands

This specification does not define:

- general language syntax outside module-related file formats
- target build-constraint syntax
- native extension ABI details

This specification is complementary to:

- `repository-layout.md`
- `native-ffi.md`

## 2. Design Principles

### 2.1 Normative Keywords

The key words **MUST**, **MUST NOT**, **REQUIRED**, **SHOULD**, **SHOULD NOT**, and **MAY** in this document are to be interpreted as normative requirements.

### 2.2 Design Principles

The Vo module system is defined by the following principles:

- **Canonical identity**. Every published module and package has exactly one canonical path. The toolchain MUST reject alternate spellings rather than normalizing them silently.
- **Deterministic builds**. A build uses an already-resolved dependency graph and MUST NOT silently change it.
- **Separation of intent and resolution**. `vo.mod` records human-authored dependency intent. `vo.lock` records the exact published graph selected from that intent.
- **Root-lock authority**. Only the root project's `vo.lock` is authoritative for a build. Dependency-local lockfiles are not consulted when a module is consumed as a dependency.
- **Frozen builds**. `vo build`, `vo check`, `vo test`, and `vo run` MUST NOT access the network, mutate `vo.mod`, mutate `vo.lock`, or re-solve dependencies.
- **Target-neutral dependency graph**. Source dependency selection is independent of native or WASM target. Target-specific artifacts supplement a resolved module version; they do not change graph identity.
- **Workspace isolation**. `vo.work` MAY replace a module's source tree locally, but it MUST NOT rewrite canonical module identity or published lockfile semantics.
- **Hard failure on integrity mismatch**. The toolchain MUST fail on lock, manifest, source, or artifact mismatches. It MUST NOT fall back to unchecked behavior.
- **GitHub-backed publication**. The publication protocol for version 1 is GitHub Releases plus a verified release manifest and source package.

## 3. Module Identity

### 3.1 Canonical Module Path

A Vo module path is a canonical GitHub path:

```text
github.com/<owner>/<repo>[/<subdir>][/vN]
```

Rules:

- A module path MUST begin with `github.com/`.
- The first three path segments MUST be exactly `github.com/<owner>/<repo>`.
- Every segment MUST match the lowercase ASCII pattern `[a-z0-9][a-z0-9._-]*`.
- Uppercase letters are not canonical and MUST be rejected.
- Empty segments, `.` segments, and `..` segments are not allowed.
- The path MUST NOT start with `/`, end with `/`, or contain `//`.
- `/<subdir>` is allowed for modules published from a repository subdirectory.
- `/vN` is the major-version suffix for incompatible major versions where `N >= 2`.
- The suffix `v0`, `v1`, and zero-padded major suffixes such as `v02` are invalid.
- The segments after `github.com/<owner>/<repo>/`, if any, identify the module root directory inside the repository.
- The canonical module path is the module's permanent identity.
- Two modules are the same module only if their canonical module paths are byte-for-byte identical.

Examples:

```text
github.com/vo-lang/vogui
github.com/vo-lang/mono/graphics
github.com/vo-lang/voplay/v2
```

### 3.2 Major Version Rule

For major version `v0` and `v1`, the canonical module path has no major suffix.
For major version `v2` and above, the canonical module path MUST end in `/vN`.

This rule is part of module identity, not merely a publishing convention:

- `github.com/acme/lib` accepts only `v0.x.y` and `v1.x.y`
- `github.com/acme/lib/v2` accepts only `v2.x.y`
- `github.com/acme/lib` and `github.com/acme/lib/v2` are distinct modules with distinct lock entries

Examples:

- `github.com/acme/lib` may publish `v0.x.y` and `v1.x.y`
- `github.com/acme/lib/v2` publishes `v2.x.y`
- `github.com/acme/lib` and `github.com/acme/lib/v2` are distinct modules

### 3.3 Package Identity

A package is identified by its full canonical import path.
A package path is either:

- `<name>` or `<name>/<subpath>` for a standard-library package
- `<module-path>` for the module root package
- `<module-path>/<package-subpath>` for a package inside a module

Standard-library package names are reserved.
Collision with non-stdlib import paths is structurally impossible because all non-stdlib packages have paths beginning with `github.com/`.

A package path is canonical only if:

- it obeys the path-shape rules above
- it does not contain `@`
- it is not relative
- if it is non-stdlib, its owning module path is canonical

Examples:

```text
fmt
encoding/json
github.com/acme/app
github.com/acme/app/internal/cache
github.com/vo-lang/vogui/widget
```

### 3.4 Repository Mapping

A canonical module path maps to a GitHub repository as follows:

- `github.com/<owner>/<repo>` identifies repository `<owner>/<repo>`
- for subpath modules such as `github.com/<owner>/<repo>/<subdir>`, the repository is still `<owner>/<repo>`
- the major-version suffix `/vN` does not change repository identity
- the module root directory inside the repository is the canonical suffix after `github.com/<owner>/<repo>/`, or `.` for a repository-root module

Examples:

- `github.com/acme/lib` -> repository `acme/lib`, module root `.`
- `github.com/acme/lib/v2` -> repository `acme/lib`, module root `v2`
- `github.com/acme/mono/graphics` -> repository `acme/mono`, module root `graphics`
- `github.com/acme/mono/graphics/v2` -> repository `acme/mono`, module root `graphics/v2`

### 3.5 Reserved Namespaces

The following path spaces are reserved by the module system:

- `std` and `std/...` are reserved for the standard library namespace and MUST NOT be used as module paths or external import prefixes
- `local` and `local/...` are reserved for ephemeral single-file modules that embed inline `vo.mod` metadata (see Section 5.6). A `local/*` path is a module-identity namespace only and MUST NOT appear in any `require` line, any published `vo.release.json`, any `vo.lock` entry, or any `import` statement
- non-stdlib, non-ephemeral module paths MUST begin with `github.com/`
- the `@` marker is not part of source-level import syntax

## 4. Imports

### 4.1 Import Forms

Vo supports the following import forms:

```go
import "fmt"
import "github.com/acme/app/util"
import util "github.com/acme/app/util"
import . "github.com/acme/app/testing"
import _ "github.com/acme/app/metrics"
```

Rules:

- Standard-library packages are imported by bare stdlib path such as `fmt` or `encoding/json`.
- All non-stdlib imports MUST use canonical package paths beginning with `github.com/`.
- Import aliases are lexical only; they do not change package identity.
- The default local name for an import is the declared package name of the imported package, not the last segment of the import path. An explicit alias overrides the default.
- Relative imports such as `"./util"` and `"../x"` are not allowed.
- Filesystem-absolute imports are not allowed.
- The `@` marker is not part of the source-language import syntax.
- A bare name or slash path that is not owned by the active standard library is an error, not a project-relative import.

### 4.2 Invalid Import Forms

The toolchain MUST reject, at minimum, the following forms:

- `import "./util"`
- `import "../shared"`
- `import "/tmp/pkg"`
- `import "std/fmt"`
- `import "github.com/acme/lib@v1.2.3"`
- `import "example.com/acme/lib"`

### 4.3 Import Classification

Import classification is purely syntactic:

- If the first path segment contains a `.`, the import is an external canonical path and MUST begin with `github.com/`.
- Otherwise, the import is a standard-library path.

This rule is authoritative for parsing, diagnostics, dependency ownership, and tooling. The implementation MUST NOT maintain separate competing classifiers for stdlib and external imports.

### 4.4 Intra-Module Imports

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

### 4.5 Standard Library Imports

Standard-library packages are imported by their bare name without a path prefix.

No third-party module path may collide with a standard-library package name.
This is structurally guaranteed by the rule that all non-stdlib module paths begin with `github.com/`.

The active standard-library set is owned by the active Vo toolchain version, not by `vo.mod` or `vo.lock`.

Examples:

```go
import "fmt"
import "io"
import "strings"
import "encoding/json"
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

`vo.mod` is UTF-8, line-oriented text.

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
- Blank lines are ignored.
- Full-line comments beginning with `//` are ignored.
- Inline trailing comments are not part of the format.
- Alias-based `require` syntax is not supported.
- `replace` directives are not supported in `vo.mod`.
- Unknown directives are errors.
- Canonical formatting writes `module`, then `vo`, then `require` lines sorted lexicographically by module path.

### 5.2 Version Constraints in `vo.mod`

A dependency `require` line uses one of the following exact or range forms:

- **Exact**: `v1.2.3`
- **Compatible**: `^1.2.3` or `^0.4.0`
- **Patch-compatible**: `~1.2.3`

The `vo` line uses the same operators but without a leading `v` in exact versions:

- **Exact**: `1.2.3`
- **Compatible**: `^1.2.3`
- **Patch-compatible**: `~1.2.3`

Normative meaning:

- `v1.2.3` means exactly `v1.2.3`
- `^1.2.3` means `>= v1.2.3` and `< v2.0.0`
- `^0.4.0` means `>= v0.4.0` and `< v0.5.0`
- `^0.0.3` means `>= v0.0.3` and `< v0.0.4`
- `~1.2.3` means `>= v1.2.3` and `< v1.3.0`

Version-format rules:

- Exact dependency versions MUST use canonical SemVer syntax `vMAJOR.MINOR.PATCH[-PRERELEASE]`.
- Exact toolchain versions and toolchain range lower bounds MUST use canonical SemVer syntax `MAJOR.MINOR.PATCH[-PRERELEASE]`.
- Build metadata (`+meta`) is not part of the module protocol and MUST be rejected in versions and constraints.
- Numeric components and numeric prerelease identifiers MUST NOT contain leading zeros.
- Pre-release versions satisfy a range only when the range lower bound explicitly names a pre-release version.

Major-path compatibility rules:

- an unsuffixed module path such as `github.com/acme/lib` accepts only `v0.x.y` and `v1.x.y`
- a suffixed module path such as `github.com/acme/lib/v2` accepts only `v2.x.y`

### 5.3 `vo.lock`

`vo.lock` is the machine-generated lock file.
It records the exact resolved dependency graph used by build commands.
It records the published registry graph, independent of any local `vo.work` overrides.

Only the root project's `vo.lock` is authoritative for a build.
If a dependency source package also contains its own `vo.lock`, that nested lockfile is ignored when the dependency is consumed.

`vo.lock` must contain, at minimum, for every resolved module:

- canonical module path
- exact resolved version
- exact locked toolchain constraint from the published manifest
- immutable source revision identity
- verified source artifact digest
- verified release-manifest digest
- direct dependency edges (which other resolved modules this module depends on)
- the full published target-artifact identity set for that module version, including `kind`, `target`, `name`, `size`, and `digest`

The dependency edges allow the toolchain to reason about the full graph without re-resolving: pruning orphaned transitive dependencies, explaining why a module is present, and detecting stale entries after a `require` removal.

A `vo.lock` file is authoritative for builds.
If `vo.mod` is human intent, `vo.lock` is the exact executable contract.

Rules:

- The root module itself MUST NOT appear in `[[resolved]]`.
- `root.module` and `root.vo` MUST exactly match the root `vo.mod`.
- `resolved` MUST contain every and only the non-root modules reachable from the root module's direct requirements.
- A canonical module path may appear at most once in `resolved`.
- `deps` MUST be unique, sorted lexicographically, and MUST equal the direct dependency module-path set declared by that module version's `vo.release.json`.
- `artifact` MUST be unique, sorted by `(kind, target, name)`, and MUST equal the published artifact set declared by that module version's `vo.release.json`, including size and digest.
- `commit` MUST be the 40-character lowercase hexadecimal Git commit recorded by the release manifest.
- `release_manifest`, `source`, and `artifact[*].digest` MUST use the digest format `sha256:<64 lowercase hex>`.

An illustrative TOML shape is:

```toml
version = 1
created_by = "vo 1.0.0"

[root]
module = "github.com/acme/app"
vo = "^1.0.0"

[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.4.2"
vo = "^1.0.0"
commit = "9b6d4a8d2d5a6f2d4c0c2d9e6b3a1f0e0c4d1e22"
release_manifest = "sha256:2f7d..."
source = "sha256:81c1..."
deps = ["github.com/vo-lang/voplay"]

[[resolved.artifact]]
kind = "extension-wasm"
target = "wasm32-unknown-unknown"
name = "vogui-wasm.wasm"
size = 123456
digest = "sha256:6f92..."
```

The exact serialization format of `vo.lock` is part of the toolchain contract and MUST be stable across compatible toolchain releases.

### 5.4 `vo.work`

`vo.work` is the local workspace override file.
It is not part of published module semantics.

Purpose:

- map canonical module paths to local source directories during development
- enable multi-module local development without changing published dependency identity

Format:

```toml
version = 1

[[use]]
path = "../vogui"

[[use]]
path = "../voplay"

[[use]]
module = "github.com/vo-lang/vopack"
path = "../vopack"
```

Equivalent explicit form:

```toml
version = 1

[[use]]
module = "github.com/vo-lang/vogui"
path = "../vogui"

[[use]]
module = "github.com/vo-lang/voplay"
path = "../voplay"
```

Rules:

- `vo.work` is UTF-8 TOML.
- `version = 1` is the workspace-file schema version, not a toolchain constraint.
- Each `[[use]]` entry maps one canonical module path to one local directory.
- `path` MAY be relative to the directory containing `vo.work` or MAY be absolute.
- If `module` is omitted, the toolchain MUST read `<path>/vo.mod` and use its `module` declaration.
- The target directory MUST contain a `vo.mod` whose `module` line matches the canonical module path for that entry.
- Two `[[use]]` entries MUST NOT resolve to the same canonical module path.
- `vo.work` affects only the local workspace where it is used.
- Published modules MUST NOT rely on consumers reading the publisher's `vo.work`.
- Tooling MUST provide a way to disable `vo.work` for CI and release workflows.

### 5.5 `vo.ext.toml`

A module may contain `vo.ext.toml` to declare extension metadata.
That file is part of the source package and versioned with the module.
Its detailed schema and ABI semantics are defined in `native-ffi.md`.

The module system treats extension metadata as part of a module's published contents, not as a separate dependency system.

Module-protocol rules:

- `vo.ext.toml` MAY declare target-specific extension assets such as WASM artifacts, native dynamic libraries, or generated bridge files.
- A published module that contains Rust-backed extension code MUST describe that extension in `vo.ext.toml`.
- If a module publishes Rust-backed extension code, `vo.ext.toml` MUST explicitly declare the set of supported published targets for that module version.
- Supported targets MUST use canonical target identifiers such as Rust target triples or `wasm32-unknown-unknown`; coarse labels such as `mac`, `linux`, or `win` are not sufficient protocol identifiers.
- A module version MAY support only a subset of targets. Omission of a target means that target is unsupported for that version.
- For every target explicitly declared as supported by a Rust-backed extension, the published release MUST include the required binary artifact set for that target.
- Removed `vo.ext.toml` schema shapes are invalid under this protocol and MUST be rejected rather than rewritten or interpreted compatibly.

### 5.6 Inline `vo.mod` Metadata

A single `.vo` source file MAY embed `vo.mod` metadata directly in its source text.
This inline form exists to let an ad hoc, single-file program declare external dependencies without creating a project directory.

See also: [`module-inline-mod-tutorial.md`](./module-inline-mod-tutorial.md) for a practical guide to writing, running, caching, and promoting inline-mod scripts.

Inline metadata produces an *ephemeral single-file module*.
It participates in the module protocol using the same rules as a regular `vo.mod`, except where explicitly narrowed by this section.

#### 5.6.1 Placement and Syntax

Inline metadata is a single block comment with the exact opening sentinel `/*vo:mod`.

Normative form:

```vo
/*vo:mod
module local/<name>
vo <toolchain-constraint>

require <module-path> <version-constraint>
...
*/

package main

func main() { ... }
```

Rules:

- The inline mod block MUST appear before the first non-whitespace, non-comment token of the file.
- Only a single inline mod block is permitted per file; a second occurrence is an error.
- The opening sentinel MUST be exactly `/*vo:mod`. A leading line break inside the block is permitted.
- The block is terminated by the first `*/` token after the opening sentinel.
- The characters between the opening sentinel and the closing `*/` are the *inline mod body*.
- The inline mod body MUST parse, verbatim, under the `vo.mod` grammar defined in Section 5.1.
- The sentinel `/*vo:mod` is reserved; a leading block comment starting with `/*vo:` but using any other directive name is an error.

#### 5.6.2 Module Identity

- The `module` line of an inline mod MUST use the reserved `local/<name>` namespace defined in Section 3.5.
- `<name>` MUST match `[a-z0-9][a-z0-9._-]*` and MUST NOT contain `/`.
- `local/<name>` identities are file-local and non-publishable. They are not a canonical path for any purpose other than naming the ephemeral module itself.
- Two inline mods with the same `local/<name>` in different files are distinct ephemeral modules.

#### 5.6.3 Content Rules

- The inline mod body follows all rules in Section 5.1 (`vo.mod`) and Section 5.2 (version constraints) except as narrowed below.
- `require` MAY name any canonical external module path. `local/*` MUST NOT appear in `require`.
- `replace` directives MUST NOT appear in an inline mod.
- Unknown directives MUST be rejected; they MUST NOT be silently ignored because the block is embedded in source.

#### 5.6.4 Precedence

- If the file's enclosing directory tree (the file itself, or any ancestor directory) contains a `vo.mod`, any inline mod block in that file MUST be rejected as an error. Project-level `vo.mod` is the single source of truth for module identity within a project.
- If no enclosing `vo.mod` exists and the file contains an inline mod block, the toolchain MUST treat that file as a single-file ephemeral module (see Section 10.2).
- If no enclosing `vo.mod` exists and the file contains no inline mod block, the toolchain MUST treat that file as an ad hoc program (see Section 10.1).

#### 5.6.5 Relationship to `vo.lock`

- An inline mod does not persist a user-visible `vo.lock`. The ephemeral module has no committed lock artifact.
- The toolchain MAY materialize a cache-local ephemeral lock for reproducibility and integrity verification. The location, naming, and format of such an ephemeral lock are implementation-defined and are not part of the module protocol.
- Every integrity rule in Section 12 applies identically to the ephemeral module's resolved graph, whether the lock is materialized on disk or only in memory.
- Dependencies of an inline mod MUST be resolved against the public registry (Section 6) using canonical module paths. They MUST NOT be resolved against repository trees, raw file URLs, or snapshot archives outside the declared release protocol.

#### 5.6.6 Workspace and Overrides

- `vo.work` MUST NOT be consulted for a single-file ephemeral module.
- An inline mod MUST NOT declare workspace overrides.

#### 5.6.7 Illustrative Example

```vo
/*vo:mod
module local/gui_chat
vo ^0.1.0

require github.com/vo-lang/vogui ^0.4.0
*/

package main

import "github.com/vo-lang/vogui"

func main() {
    vogui.Run()
}
```

## 6. GitHub Registry Model

### 6.1 Registry Source

The Vo module registry is GitHub Releases.
A published module version is resolved from GitHub by canonical module path and exact version.

Each published module version must map to:

- an immutable Git tag
- a GitHub Release for that tag
- a release manifest asset
- a source-package asset

A version is published only if all required publication artifacts exist and pass validation. A tag by itself is not a valid module release.

### 6.2 Release Manifest

Each release must provide a machine-readable release manifest asset named `vo.release.json`.
The release manifest must contain, at minimum:

- `schema_version`: manifest schema version, currently `1`
- `module`: canonical module path
- `version`: exact module version
- `commit`: immutable Git revision
- `module_root`: module root directory relative to repository root (`.` for repository-root modules, e.g. `graphics` or `graphics/v2` for nested modules)
- `vo`: toolchain constraint from `vo.mod`
- `require`: direct dependency constraints from `vo.mod`
- `source`: source-package asset name, size, and digest
- `artifacts`: target-specific artifact metadata and digests; this list MAY be empty for pure-source modules but MUST include every published artifact required by the module's declared target-support contract

The release manifest is registry metadata.
It allows dependency resolution and integrity verification without interpreting arbitrary repository layout as package protocol.

Rules:

- `schema_version` MUST be an integer and MUST currently equal `1`.
- `module`, `version`, `vo`, `require`, `source`, and `artifacts` are authoritative published metadata.
- `require` MUST list only direct dependencies.
- `require` MUST be unique and sorted by module path.
- `artifacts` MUST be unique and sorted by `(kind, target, name)`.
- If the packaged module contains `vo.ext.toml`, the published `artifacts` set MUST satisfy the declared target-support contract for that module version.
- `module_root` MUST match the canonical module-path suffix inside the backing repository.

### 6.3 Source Package

Each release must provide a canonical source-package asset.
The source package is the authoritative published source for the module version.

Format:

- The source package is a gzip-compressed tar archive (`.tar.gz`).
- The exact asset name is declared by `vo.release.json`; the asset name itself is not part of module identity.
- The archive must unpack into a single top-level directory.
- The archive contains the contents of the module root only, not the whole repository, unless the module root is the repository root.

Content rules:

- The source package must include `vo.mod` at the archive root.
- If present in the module, it must include `vo.lock` and `vo.ext.toml`.
- The source package must include all `.vo` files required to build the module.
- The source package must not include build artifacts, caches, or version-control metadata.
- The source package digest must match both the release manifest and `vo.lock`.
- The `module` line in the packaged `vo.mod` must match the `module` field in `vo.release.json`.
- The packaged `vo.mod` `vo` line and `require` set MUST match the release manifest.
- If the packaged source contains `vo.ext.toml`, its declared published target-support set MUST be consistent with the artifacts recorded in `vo.release.json`.
- If the source package contains a dependency-local `vo.lock`, consumers MUST ignore it when using this module as a dependency.

### 6.4 Target-Specific Artifacts

A release may provide target-specific binary artifacts.
If a module's published extension metadata declares support for a target-specific runtime artifact, the corresponding published artifacts are required for that target.
Examples include:

- prebuilt native extension libraries
- WASM extension binaries
- generated bridge files required by a target runtime

Rules:

- Target-specific artifacts must be listed in `vo.release.json`.
- Artifact targets MUST use canonical target identifiers. For Rust-backed native binaries, the identifier MUST be the Rust target triple. For WASM artifacts, the identifier MUST be `wasm32-unknown-unknown`.
- A module version MAY support only a subset of targets. Unsupported targets simply do not appear in the declared target-support set.
- If `vo.ext.toml` declares support for a target that requires a published binary artifact, `vo.release.json` MUST include that artifact set for the same target.
- Binary artifacts must be verified by size and digest before use.
- Binary artifacts do not change module identity or dependency resolution.
- A module remains source-defined even when binary artifacts are present.

### 6.5 Repository Mapping and Version Discovery

A canonical module path maps to a GitHub repository as follows:

- `github.com/<owner>/<repo>` identifies repository `<owner>/<repo>`.
- For subpath modules such as `github.com/<owner>/<repo>/<subdir>`, the repository is still `<owner>/<repo>`.
- The major-version suffix `/vN` does not change the repository identity.
- The module root directory inside that repository is the canonical module-path suffix after `github.com/<owner>/<repo>/`, or `.` for a repository-root module.

Version discovery rules:

- The toolchain enumerates available versions from Git tags in the identified repository.
- For root modules, tags use the form `vX.Y.Z`.
- For non-root modules, tags use the form `<module-root>/vX.Y.Z`, where `<module-root>` is the module root directory relative to the repository root.
- A tag must have a corresponding GitHub Release with the required release manifest and source-package assets to be considered a valid published version.
- Tags that do not follow canonical semantic versioning are ignored.
- Tags whose major version does not match the canonical module path are ignored.

Examples:

- `github.com/acme/lib` -> tag `v1.4.2`
- `github.com/acme/lib/v2` -> tag `v2/v2.0.1`
- `github.com/acme/mono/graphics` -> tag `graphics/v0.8.0`
- `github.com/acme/mono/graphics/v2` -> tag `graphics/v2/v2.1.0`

## 7. Dependency Resolution

### 7.1 Resolution Inputs

Resolution takes the following inputs:

- the root module's `vo.mod`
- registry metadata from `vo.release.json`
- the set of versions available in GitHub Releases for each required module path
- optionally, the existing root `vo.lock` as an explicit preference input for targeted update commands only

`vo.work` is not a graph-resolution input. Workspace overrides replace source trees after the published graph has already been selected.

### 7.2 Resolution Output

Resolution produces a complete module graph with:

- exactly one resolved version per canonical module path
- verified registry metadata for each module
- exact source artifact digests
- the full published artifact metadata set for each locked module version
- exact direct dependency edges for every locked module

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

Additional rules:

- Unsuffixed module paths MUST NOT resolve to `v2+` releases.
- Suffixed module paths such as `.../v2` MUST resolve only to releases whose semantic major version is `2`.
- A version with invalid or inconsistent release metadata is not a candidate.

### 7.5 When Resolution Happens

Dependency resolution is performed by explicit module-management commands.
Build commands do not silently re-resolve the graph.

The toolchain must provide commands equivalent in behavior to:

- `vo mod add`
- `vo mod update`
- `vo mod sync`
- `vo mod download`

Only `vo mod add`, `vo mod update`, `vo mod sync`, and `vo mod remove` are resolution-changing commands.
`vo mod download` materializes a graph that is already pinned by `vo.lock`; it does not select new versions.

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
- no fallback from failed integrity checks to unchecked source trees or raw repository fetches

### 8.2 Missing Lock or Missing Artifacts

If a build requires external modules and `vo.lock` is missing, the build fails and instructs the user to run a module-management command.

If `vo.lock` exists but required artifacts are missing from cache, the build fails and instructs the user to fetch them explicitly.

If a build uses workspace overrides for external modules, the root `vo.lock` is still required because workspace overrides do not replace published graph authority.

### 8.3 Cache Model

The cache is an implementation detail.
It is not the source of truth for dependency identity.

Rules:

- The source of truth for builds is the root `vo.lock`.
- Cache entries SHOULD be keyed by verified module path, exact version, and content identity.
- A cached source tree or artifact is valid only when it matches the exact locked version and the exact locked digest values.
- The toolchain MAY use a global cache, a project-local cache, or both.
- Cache layout is not part of the module identity contract.
- If a cached module or artifact fails validation against `vo.lock`, the implementation MUST treat it as missing.

### 8.4 Published Native Artifacts

Published modules are source-defined, but a module version MAY publish target-specific binary artifacts such as native extension libraries.
For modules that declare supported Rust-backed published targets, those binary artifacts are part of the release contract for each declared target.

Rules:

- Frozen builds MAY compile pure Vo source from a verified locked source package.
- If a frozen build requires a published target-specific binary artifact, the toolchain MUST use the exact locked published artifact.
- If the active build target requires a published dependency's target-specific binary artifact and that dependency version does not declare support for the active target, the build MUST fail rather than attempting an implicit local native build.
- If a published dependency declares support for the active target but the required locked artifact is absent or invalid, the build MUST fail.
- A frozen build MUST NOT build a published dependency's native extension from source as an implicit fallback.
- Local workspace override modules MAY build local artifacts from local source because they are not acting as published registry materializations.

## 9. Package Ownership and Import Resolution

### 9.1 Resolution Algorithm

For an import path `P`:

1. If the first path segment of `P` does not contain `.`, resolve `P` as a standard-library package. If no such stdlib package exists, the import is an error.
2. If `P` is within the root module's canonical path space, the root module owns `P`.
3. Otherwise, if an active workspace override module path is the longest prefix-boundary match for `P`, that override module owns `P`.
4. Otherwise, find the locked module whose canonical module path is the longest prefix-boundary match for `P`.
5. The remainder of `P` after the owning module path is the package subpath inside that module.
6. Resolve that directory inside the selected source tree.
7. If no owner exists, the import is an error.

Ownership is determined by canonical import paths, never by local filesystem layout.

Examples:

- `fmt` resolves to the standard library.
- `encoding/json` resolves to the standard library.
- `github.com/acme/app/util` resolves to package `util` inside module `github.com/acme/app`.
- `github.com/vo-lang/vogui/widget` resolves inside module `github.com/vo-lang/vogui`.

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
- The default local import name is the imported package's declared package name.

### 9.4 Internal Packages

Vo supports Go-style internal visibility.

Rule:

- If an import path contains `/internal/`, that package may be imported only by packages whose canonical import path shares the parent prefix before `/internal/`.

This rule is evaluated on canonical import paths. Workspace overrides do not bypass it.

Example:

- internal package: `github.com/acme/app/internal/cache`
- allowed importer: `github.com/acme/app/cmd/tool`
- disallowed importer: `github.com/other/project/tool`

### 9.5 Build Constraints

Vo supports build constraints for target-conditional compilation.
Build constraint syntax and semantics are defined in the language specification.

The module system does not alter build constraint behavior; it applies uniformly to all packages regardless of their origin (stdlib, intra-module, or external dependency).

## 10. Ad Hoc Programs and Single-File Ephemeral Modules

A `.vo` source file that is not contained in a project with `vo.mod` is not a published module.
It is either an *ad hoc program* (no module metadata of any kind) or a *single-file ephemeral module* (declares its own dependencies via inline `vo.mod` metadata, see Section 5.6).

The toolchain MUST classify such a file using the precedence defined in Section 5.6.4.

### 10.1 Ad Hoc Programs

A `.vo` file that is not inside any project with `vo.mod` and that contains no inline mod block is an ad hoc program.

Rules:

- It MAY import only standard-library packages.
- It MUST NOT declare external module dependencies.
- It MUST NOT import sibling directories as packages.
- It MUST NOT use `vo.work`.
- Any multi-package program or program with external dependencies MUST either define a project-level `vo.mod` or embed an inline mod per Section 5.6.

### 10.2 Single-File Ephemeral Modules

A `.vo` file that is not inside any project with `vo.mod` and that contains a valid inline mod block (Section 5.6) is a single-file ephemeral module.

Rules:

- The ephemeral module's identity is the `module local/<name>` line of the inline mod.
- The ephemeral module MAY declare external dependencies via `require` lines in the inline mod.
- External dependencies MUST be canonical module paths and MUST resolve against the public registry per Section 6.
- The ephemeral module MUST NOT be imported by any other module.
- The ephemeral module MUST NOT contain or reference `vo.work`.
- Build and run commands (Section 8.1) MUST remain frozen with respect to the ephemeral module's resolved graph.
- A toolchain MAY offer a single-file auto-install entry point that resolves the inline module's `require` set before invoking the frozen build for that file.
- If the toolchain does not offer such an entry point and the graph has not yet been resolved, the run/build command MUST fail and instruct the user to run a resolution command.
- An integrity failure against the ephemeral module's resolved graph MUST fail hard. It MUST NOT fall back to raw repository fetches, uncached source snapshots, or alternate unpublished artifact sources.

### 10.3 Scope of Ephemerality

A single-file ephemeral module is *ephemeral* only in the sense that it has no on-disk `vo.mod` / `vo.lock` files owned by the user:

- all module-protocol integrity, publication, and resolution rules still apply
- the toolchain MAY materialize a cache-local ephemeral lock as described in Section 5.6.5
- the ephemeral module's dependencies are full first-class locked modules with verified digests, identical to those used by regular projects

## 11. Workspace Overrides

### 11.1 Override Semantics

When `vo.work` is active, the toolchain may replace a resolved module's registry source tree with a local directory for development.

Rules:

- The local directory must identify the same canonical module path.
- The override does not rename the module.
- Import paths remain canonical.
- The override is local state and must not be assumed by downstream consumers.
- The override replaces source materialization only; it does not participate in version solving.

Workspace discovery rules:

- A build has at most one active `vo.work`: the nearest ancestor `vo.work` of the root project directory.
- Nested `vo.work` files inside dependency trees are ignored.
- The root module MUST NOT override itself via `vo.work`.

### 11.2 Interaction with Lock Files

A workspace-aware toolchain may build from a local override directory after verifying that the override declares the same canonical module path.
However:

- `vo.lock` remains the authoritative published graph and must continue to record the registry-resolved module path, version, revision, and verified digests
- local overrides do not rewrite the module identity, selected version, or digest fields recorded in `vo.lock`
- a published module's committed `vo.lock` must be generated with workspace overrides disabled
- CI and release workflows must provide a mode that ignores `vo.work`

Additional rules:

- The consuming build MAY read the override directory's `vo.mod` only to verify canonical module identity.
- The consuming build MUST NOT import new published dependencies from an override module's local `vo.mod`.
- The consuming build MUST NOT consult an override module's nested `vo.work` or nested `vo.lock`.
- If override source imports an external package that is not owned by the root module, another active override, or the root `vo.lock`, the build fails.

This keeps local development convenient without leaking local paths into published module semantics.

## 12. Integrity Rules

### 12.1 Verification Requirements

The toolchain must verify:

- release-manifest digest
- source-package digest
- any used target-artifact digest
- module path and version consistency across `vo.mod`, `vo.lock`, Git tag, and release manifest
- root `vo.mod` vs root `vo.lock` equality for `module` and `vo`
- locked module `deps` equality with `vo.release.json.require[*].module`
- locked module artifact equality with `vo.release.json.artifacts`
- packaged `vo.ext.toml` consistency with the published target-support contract and `vo.release.json.artifacts`, when `vo.ext.toml` is present
- packaged `vo.mod` consistency with `vo.release.json`

Any mismatch is a hard error.

The implementation MUST fail immediately on integrity mismatch. It MUST NOT fall back to a weaker validation mode.

### 12.2 No Implicit Trust in Repository Trees

The module protocol is based on release metadata and verified artifacts.
A toolchain must not treat arbitrary repository trees, raw file URLs, or archive snapshots outside the declared release protocol as equivalent to a published module release.

### 12.3 Root-Lock Authority

Only the root module's `vo.lock` is authoritative for a build.
Dependency-local lockfiles may be present in published source packages or local override directories, but they do not alter the consuming build's graph.

## 13. CLI Behavior

The toolchain must expose module-management behavior equivalent to the following commands.
The exact command spelling may differ, but the semantics are normative.

### 13.1 `vo mod init <module-path>`

Creates a new `vo.mod` for the current module.

### 13.2 `vo mod add <module-path>[@constraint]`

Adds or updates a direct dependency constraint in `vo.mod`.
This command refreshes `vo.lock`.

If `@constraint` is omitted, the command resolves the latest non-prerelease published version and writes the equivalent compatible constraint as the direct requirement. If only pre-release versions exist, the command requires an explicit constraint.

### 13.3 `vo mod update [module-path]`

Re-solves dependency constraints, usually selecting newer compatible versions.
Updates `vo.lock`.

With an explicit `module-path`, the command SHOULD preserve unrelated locked versions when they still satisfy all constraints. It MAY update any shared transitive dependency required to reach a consistent graph.

### 13.4 `vo mod sync`

Recomputes the full dependency graph from `vo.mod` and writes a fresh `vo.lock`.
It verifies registry metadata and writes the full published artifact set for every locked module.

### 13.5 `vo mod download`

Fetches artifacts already pinned by `vo.lock` into cache without changing the resolved graph.

This command MAY materialize source packages and target-specific artifacts, but it MUST NOT re-solve dependencies or rewrite `vo.lock`.

### 13.6 `vo mod verify`

Verifies root `vo.mod` and `vo.lock` consistency and verifies that any already-materialized cached source or artifacts still match the lockfile.

### 13.7 `vo mod remove <module-path>`

Removes a direct dependency from `vo.mod`.
This command refreshes `vo.lock` to prune orphaned transitive dependencies.

### 13.8 `vo build`, `vo check`, `vo test`, `vo run`

Use the exact graph pinned in `vo.lock`.
Do not access the network.
Do not mutate `vo.mod` or `vo.lock`.

## 14. Not Supported by Design

The following are not part of the Vo module system:

- dependency aliases such as `@"name"`
- project-relative package imports
- filesystem-absolute package imports
- `std/...` import prefixes
- published `replace` directives inside `vo.mod`
- multiple exact versions of the same canonical module path in one build graph
- dependency-local lockfiles influencing a consuming build's dependency graph
- source-building published dependency native artifacts during frozen builds
- implicit registry access during build commands
- interpreting arbitrary GitHub repository layout as the package protocol

## 15. Typical Errors

### 15.1 Missing Lock File

```text
error: this build imports external modules but vo.lock is missing
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
error: import path "github.com/acme/unknown/pkg" is not owned by any resolved module
```

### 15.4 Locked Manifest Mismatch

```text
error: vo.lock entry does not match published release manifest
  module: github.com/acme/lib
  version: v1.2.3
  field: deps
```

### 15.5 Missing Locked Artifact

```text
error: required locked artifact is missing from cache
  module: github.com/acme/lib
  version: v1.2.3
  target: aarch64-apple-darwin
  run: vo mod download
```

### 15.6 Internal Package Violation

```text
error: use of internal package not allowed
  github.com/other/project/tool cannot import
  github.com/acme/app/internal/cache
```

### 15.7 Workspace Identity Mismatch

```text
error: workspace override for github.com/vo-lang/vogui points to ../vogui-dev
  expected module path: github.com/vo-lang/vogui
  found in vo.mod:     github.com/acme/forked-vogui
```

### 15.8 Override Introduces Unlocked Dependency

```text
error: workspace override imports an external module that is not present in the root lockfile
  importer: github.com/acme/app/devtools
  import:   github.com/acme/newdep
  run: vo mod sync (from the root project) or disable vo.work
```

### 15.9 Missing Required Published Target Artifact

```text
error: vo.release.json is missing a required published artifact
  module: github.com/acme/lib
  version: v1.2.3
  target: x86_64-unknown-linux-gnu
  declared by: vo.ext.toml
```

## 16. Related Specifications

- `repository-layout.md`
- `native-ffi.md`
