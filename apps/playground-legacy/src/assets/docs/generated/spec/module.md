<!--
Generated from lang/docs/spec/module.md
Generator: node scripts/ci/docs_sync.mjs
Source-Digest: sha256:7db69c4a515313e66e800ea9432c7a868591d7df44273c63bcd536324f4346e2
Generated-At: 2026-05-27T11:17:51+08:00
-->
# Vo Module Specification

Version: 1.0
Status: Draft

## 1. Scope

This specification defines the normative Vo module protocol. It fixes the behavior of:

- canonical module and package identity
- import-path classification and ownership
- semantic-version syntax and constraint semantics
- `vo.mod`, `vo.lock`, `vo.work`, `vo.release.json`, and `vo.web.json`
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
- **Single authored module manifest**. `vo.mod` records human-authored dependency intent, extension metadata, web runtime metadata, and declared publication targets. Other module metadata files are generated from it.
- **Separation of intent and resolution**. `vo.mod` records human-authored intent. `vo.lock` records the exact published graph selected from that intent.
- **Root-lock authority**. Only the root project's `vo.lock` is authoritative for a build. Dependency-local lockfiles are not consulted when a module is consumed as a dependency.
- **Frozen builds**. `vo build`, `vo check`, `vo test`, and `vo run` use an
  already-pinned graph. Current public CLI entry points may materialize
  already-locked dependencies into the cache, but they must not mutate
  `vo.mod`, mutate `vo.lock`, re-solve dependencies, or silently upgrade the
  graph.
- **Target-neutral dependency graph**. Source dependency selection is independent of native or WASM target. Target-specific artifacts supplement a resolved module version; they do not change graph identity.
- **Workspace isolation**. `vo.work` MAY replace a module's source tree locally, but it MUST NOT rewrite canonical module identity or published lockfile semantics.
- **Hard failure on integrity mismatch**. The toolchain MUST fail on lock, manifest, source, or artifact mismatches. It MUST NOT fall back to unchecked behavior.
- **Generated consumption indexes**. `vo.release.json` is the complete release index for CLI and native tooling. `vo.web.json` is the browser index for Studio web and other web runtimes.
- **GitHub-backed publication**. The publication protocol for version 1 is GitHub Releases plus verified generated manifests, a source package, and declared artifacts.

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
- Every segment MUST also be a portable filesystem component: it MUST be at most 255 UTF-8 bytes, MUST NOT end in `.` or whitespace, and MUST NOT use a reserved platform device name.
- Replacing each `/` with `@` in the complete module path MUST produce one portable component of at most 255 UTF-8 bytes; this is the canonical flat cache key.
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

- it is at most 4096 UTF-8 bytes, contains at most 256 `/`-separated
  components, and every component is at most 255 UTF-8 bytes
- every component uses NFC Unicode normalization and is a portable filesystem
  component: it is non-empty, is neither `.` nor `..`, has no control
  character or `<>:"|?*\\`, does not begin or end with whitespace, does not
  end in `.`, and is not a reserved platform device name
- it contains no empty component, repeated separator, or trailing separator
- it does not contain `@`
- it is not relative
- if it is non-stdlib, its first three components form the canonical
  `github.com/<owner>/<repo>` root and its owning module path is canonical
- package-subpath components after the owning module path are exact,
  case-sensitive NFC strings and may contain portable Unicode; the lowercase
  ASCII restriction applies to the owning module-path prefix

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

### 3.6 Portable Path Unicode Profile

Every path described by this specification as a portable path or portable path
component uses Unicode 16.0 data. Each non-ASCII component MUST already be in
Unicode 16.0 NFC; implementations MUST reject a non-NFC spelling instead of
normalizing it implicitly. Collision detection MUST derive each component key
by applying Unicode 16.0 full case folding followed by Unicode 16.0 NFC. The
original NFC spelling remains the canonical wire spelling. These rules apply
uniformly to source paths, include paths, extension and artifact paths, archive
entries, and every other module-relative path carried by module metadata. One
portable relative path MUST contain at most 256 components.

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
- `import "local/demo"`
- `import "github.com/acme/lib@v1.2.3"`
- `import "example.com/acme/lib"`
- `import "github.com/acme/lib/../other"`
- `import "github.com/acme/lib//util"`
- any path containing a non-NFC, non-portable, or oversized component

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
- optional source files or directories that must be published with the module
- optional extension metadata
- optional browser runtime metadata
- optional WASM and native target artifact declarations

`vo.mod` does not contain:

- transitive dependency resolution results
- local workspace overrides
- checksums for the resolved graph
- generated source-file listings
- generated release artifact digests

`vo.mod` is UTF-8 text. Its top-level module directives are line-oriented.
Optional metadata sections use TOML table syntax after the top-level directives.

Format:

```text
module <module-path>
vo <toolchain-constraint>

require <module-path> <version-constraint>
require <module-path> <version-constraint>
...

[web]
entry = "main.vo"
include = ["assets"]

[extension]
name = "module_extension_name"
include = ["js/dist"]

[extension.wasm]
type = "standalone"
wasm = "artifact.wasm"
local_wasm = "web-artifacts/artifact.wasm"

[extension.web]
entry = "Run"
capabilities = ["widget", "vo_web"]

[extension.web.js]
renderer = "js/dist/renderer.js"

[extension.native]
path = "rust/target/{target}/release/{library}"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libmodule.dylib"
```

Example:

```text
module github.com/acme/app
vo ^1.0.0

require github.com/vo-lang/vogui ^0.4.0
require github.com/vo-lang/voplay ~0.7.2
require github.com/acme/http v1.3.1

[web]
entry = "main.vo"
include = ["assets"]
```

Rules:

- There must be exactly one `module` line.
- There must be exactly one `vo` line.
- `require` entries declare direct dependencies only.
- Metadata sections are optional.
- A given module path may appear at most once in the `require` list.
- A dependency is identified only by canonical module path, never by alias.
- Blank lines are ignored outside metadata sections.
- Full-line comments beginning with `//` are ignored outside metadata sections.
- Outside metadata sections, horizontal whitespace is ASCII space or tab.
  Unicode whitespace is data and MUST be rejected rather than silently trimmed
  or treated as a directive separator.
- Metadata sections follow TOML comment and value syntax.
- Inline trailing comments are not part of top-level directive syntax.
- Alias-based `require` syntax is not supported.
- `replace` directives are not supported in `vo.mod`.
- Unknown top-level directives and unknown metadata tables are errors.
- Canonical formatting writes `module`, then `vo`, then `require` lines sorted lexicographically by module path, followed by metadata tables in canonical table order.

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
- Before parsing or compiling project source, the active compiler version MUST satisfy the root `vo` constraint. An incompatible project or inline module is a hard error that reports both the required constraint and active version.
- Build metadata (`+meta`) is not part of the module protocol and MUST be rejected in versions and constraints.
- Numeric components and numeric prerelease identifiers MUST NOT contain leading zeros.
- A SemVer without its dependency-version `v` prefix MUST be at most 254 UTF-8 bytes, so an exact dependency version fits one 255-byte portable cache component.
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
- direct dependency requirement edges, each recording the canonical dependency module path and the exact published constraint
- the full published target-artifact identity set for that module version, including `kind`, `target`, `name`, `size`, and `digest`

The dependency edges allow the toolchain to reason about the full graph without re-resolving: pruning orphaned transitive dependencies, explaining why a module is present, and detecting stale entries after a `require` removal.

A `vo.lock` file is authoritative for builds.
If `vo.mod` is human intent, `vo.lock` is the exact executable contract.

Rules:

- The root module itself MUST NOT appear in `[[resolved]]`.
- `root.module` and `root.vo` MUST exactly match the root `vo.mod`.
- `resolved` MUST contain every and only the non-root modules reachable from the root module's direct requirements.
- `resolved` MUST be strictly sorted lexicographically by canonical module path, so a canonical module path appears at most once.
- `deps` MUST be unique by module path, sorted lexicographically by module path, and MUST exactly equal the `(module, constraint)` requirement set declared by that module version's `vo.release.json`.
- Every locked dependency edge's `constraint` MUST accept the single selected version of its target module.
- Every dependency module's locked `vo` constraint MUST cover the root lock's complete `root.vo` range.
- A dependency edge whose target is absent, a duplicate selection, conflicting selections for one module path, an unsatisfied edge, or an unreachable selected module is a hard lock-integrity error.
- `artifact` MUST be unique, sorted by `(kind, target, name)`, and MUST equal the published artifact set declared by that module version's `vo.release.json`, including size and digest.
- `commit` MUST be the 40-character lowercase hexadecimal Git commit recorded by the release manifest.
- `release_manifest`, `source`, and `artifact[*].digest` MUST use the digest format `sha256:<64 lowercase hex>`.

An illustrative TOML shape is:

```toml
version = 2
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
deps = [
  { module = "github.com/vo-lang/voplay", constraint = "^0.7.0" },
]

[[resolved.artifact]]
kind = "extension-wasm"
target = "wasm32-unknown-unknown"
name = "vogui-wasm.wasm"
size = 123456
digest = "sha256:6f92..."

[[resolved]]
path = "github.com/vo-lang/voplay"
version = "v0.7.3"
vo = "^1.0.0"
commit = "1111111111111111111111111111111111111111"
release_manifest = "sha256:aaaa..."
source = "sha256:bbbb..."
deps = []
```

`version = 2` is the lock-file schema version. Version 1 path-only dependency
edges are rejected because they cannot prove transitive constraint integrity.
Each `deps` element in version 2 is an inline table containing exactly
`module` and `constraint`; unknown fields are rejected.

The exact serialization format of `vo.lock` is part of the toolchain contract and MUST be stable across compatible toolchain releases.
Canonical `vo.lock` input and output MUST NOT exceed 128 MiB. This dedicated
limit permits the maximum bounded graph metadata to round-trip even when module
paths, constraints, and artifact identities approach their portable wire
limits; generic source-text limits do not apply to `vo.lock`.

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
- With `VOWORK` unset, tooling selects the nearest ancestor `vo.work` of the
  root project directory.
- Exact `VOWORK=off` disables workspace discovery for the complete command.
- Any other non-empty `VOWORK` value explicitly selects that workspace file.
  A relative value is resolved against the root project directory. An empty
  value, a missing path, a directory, or a malformed file is a hard workspace
  error; tooling MUST NOT fall back to ancestor discovery.
- One resolved workspace policy and selected-file identity MUST propagate
  through dependency loading, source analysis, native-extension builds, and
  proc-macro expansion. A nested Cargo invocation MUST NOT rediscover a
  different workspace from process state.

### 5.5 Extension, Web, and Native Metadata in `vo.mod`

Extension, web, and native publication metadata are declared in `vo.mod`.
The module protocol defines one human-authored module configuration file.

The detailed native ABI semantics are defined in `native-ffi.md`.
This specification defines only the module-level declaration and publication contract.

#### 5.5.1 Source Includes

`include` entries under `[web]` or `[extension]` declare non-`.vo` files or directories that are part of the published module source for the corresponding runtime.

Rules:

- Include paths MUST be relative paths inside the module root.
- Include paths MUST NOT be absolute.
- Include paths MUST NOT contain empty, `.`, or `..` path components.
- Include paths MUST be committed module contents at the published revision.
- A release command MUST fail if a declared include path does not exist.

#### 5.5.2 Browser Project Metadata

`[web]` declares that the module can be opened or run by a browser runtime.

Fields:

- `entry`: optional browser entry `.vo` file or exported entry name, depending on the consuming runtime
- `include`: optional list of source files or directories needed by the browser runtime

Rules:

- Browser metadata MUST describe source and web runtime inputs only.
- Browser metadata MUST NOT declare native dynamic libraries.
- Browser consumers MUST use `vo.web.json` rather than interpreting repository layout directly.

#### 5.5.3 Extension Metadata

`[extension]` declares extension identity and shared extension source includes.

Fields:

- `name`: required extension name
- `include`: optional list of source files or directories needed by extension runtimes

Rules:

- A module that publishes extension artifacts MUST declare `[extension]`.
- Extension metadata is part of the module's published contents.
- Extension metadata does not create a separate dependency graph.
- The extension `name` identifies an artifact/provider. Source extern symbols
  use each package's complete canonical import path plus its exact function
  identifier, so changing `name` does not rename language symbols.
- Every extension backend selects the longest loaded canonical module owner
  for a package before looking up an exact extern function. If that owner does
  not export the function, resolution reports a missing provider and does not
  fall back to a parent module's artifact.
- One exact canonical owner has one live artifact/provider boundary. Native
  linkme and dynamic-library catalogs cannot split the same owner; browser
  artifacts require explicit disposal before a different generation can claim
  the owner. Owner-catalog changes after VM resolution require a new VM load.

#### 5.5.4 WASM Artifacts

`[extension.wasm]` declares browser-loadable WASM artifacts.

Fields:

- `type`: required, either `"standalone"` or `"bindgen"`
- `wasm`: required logical artifact filename for the WASM module
- `js_glue`: required logical artifact filename for `"bindgen"`, absent for `"standalone"`
- `local_wasm`: optional repository-relative path used by browser raw-file loading
- `local_js_glue`: optional repository-relative path used by browser raw-file loading

Rules:

- A declared WASM artifact target is `wasm32-unknown-unknown`.
- If `type = "bindgen"`, both `wasm` and `js_glue` MUST be declared.
- If `local_wasm` or `local_js_glue` is declared, that path MUST exist at the published revision.
- `vo.release.json` MUST include the declared WASM and JS glue artifacts.
- `vo.web.json` MUST include only the browser-loadable artifact paths and metadata needed by web consumers.
- The resolved canonical module path is the artifact's extern-owner identity.
  `[extension].name`, `wasm`, and `js_glue` MUST NOT create owner aliases.
- An owner claims only its exact root package and descendant packages separated
  by `/`. Descendant segments are case-sensitive and may contain portable
  Unicode. Empty segments, `.`/`..`, boundary Unicode whitespace, trailing
  dots, slashes, backslashes, `@`, control characters, Windows-forbidden
  punctuation, reserved Windows device stems, segments above 255 UTF-8 bytes,
  and package paths outside the 4096-byte extern wire limit are outside that
  ownership boundary. Browser dispatch uses the shared longest-owner rule when
  nested loaded modules both match a package.
- Every final standalone or bindgen artifact MUST export
  `vo_ext_protocol_version()` and return browser protocol version `3` before
  the host publishes that artifact for dispatch. Bindgen hosts validate the
  underlying instance exports returned by initialization.
- The root Rust crate producing a final `.wasm` artifact SHOULD invoke
  `vo_ext::export_wasm_extension_protocol!()` exactly once. Linked dependency
  crates MUST NOT emit a second protocol export.
- Browser extern dispatch MUST decode the canonical length-coded
  `(package, function)` identity and call the export named `__vo_ext_` followed
  by the lowercase hexadecimal form of every UTF-8 byte in the complete
  canonical encoded extern name. The key has no hash or truncation. Standalone
  and bindgen artifacts use the same key. Decoded-function, full-wire-name,
  flattened-package, and textual-prefix fallbacks are invalid. A missing exact
  key at the deepest owner is an artifact contract failure and MUST NOT fall
  back to a parent owner.
- Reloading byte-identical WASM with byte-identical decoded JS glue source is
  idempotent; a transport URL does not define artifact identity.
  A different artifact under a live owner MUST be rejected without replacing
  it; explicit disposal is required before intentional replacement. Concurrent
  identical loads join one transaction, and disposal or reset prevents a
  pending transaction from publishing stale state. A prepared artifact remains
  outside active dispatch maps until Rust owner registration and a synchronous
  JavaScript commit complete in one continuation. Each waiter owns a lease;
  cancellation of the final uncommitted lease destroys the prepared artifact.
  The host retains opaque handle identity privately, allowing malformed public
  setup-handle fields to cancel the exact lease without trusting those fields.
  Lifecycle and lease counters never wrap; exhaustion rejects publication, and
  an owner whose final-lease cancellation exhausts its generation remains
  unavailable until a successful whole-runtime reset.
- Every successful artifact publication receives a monotonic generation. A VM
  freezes the selected `(owner, generation)` for each extension extern and MUST
  reject calls or replays after the deepest owner changes, the owner is
  disposed, or the same owner is replaced. The host rebuilds the VM to bind the
  new artifact. The browser bridge validates the frozen binding before and
  after every JavaScript export and discards output from a call that changed
  its own lifecycle. Changes to unrelated owners leave the binding valid.
- Lifecycle counters are preflighted before an owner transaction. Rust
  owner-generation removal MUST succeed before pending loads, generations, or
  active JavaScript dispatch maps are changed. A Rust removal error leaves the
  complete JavaScript transaction routable and retryable; successful removal
  is followed by non-throwing map deletion and best-effort resource cleanup.
  Nested VM save/restore scopes cannot load, dispose, or reset extension
  artifacts.
- The value, control-frame, memory-ownership, replay, and cache-epoch contract
  is defined by browser WASM protocol v3 in `native-ffi.md` section 6.

#### 5.5.5 Extension Web Runtime Metadata

`[extension.web]` and `[extension.web.js]` declare browser runtime behavior for an extension.

Fields:

- `[extension.web].entry`: optional exported entry name
- `[extension.web].capabilities`: optional list of runtime capabilities
- `[extension.web.js]`: named JavaScript runtime modules, such as `renderer`, `protocol`, or `host_bridge`

Rules:

- JavaScript runtime module paths MUST be relative paths inside the module root.
- JavaScript runtime module paths MUST be included in the published source set.
- Browser consumers load these files through `vo.web.json`; they do not infer them from repository layout.

#### 5.5.6 Native Artifacts

`[extension.native]` declares optional published native dynamic-library artifacts.

Fields:

- `path`: optional local build-output pattern used by release tooling
- `[[extension.native.targets]]`: zero or more supported native targets
- `target`: required Rust target triple for each declared native target
- `library`: required logical artifact filename for that target

Rules:

- Native support is optional. A module with no `[extension.native]` target declarations publishes no native artifacts.
- If a native target is declared, the release for that module version MUST include the corresponding native artifact.
- A declared native target MUST use a canonical Rust target triple such as `aarch64-apple-darwin`, `x86_64-unknown-linux-gnu`, or `x86_64-pc-windows-msvc`.
- Coarse labels such as `mac`, `linux`, or `win` are invalid target identifiers.
- Native dynamic-library artifact names SHOULD use the platform's normal extension, such as `.dylib`, `.so`, or `.dll`.
- `vo.release.json` MUST include every declared native artifact with size and digest.
- `vo.web.json` MUST NOT include native artifacts.
- Browser runtimes MUST NOT load or reason about native artifacts.
- A module version MAY support only a subset of native targets. Omission of a target means that target is unsupported for that version.

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
- `<name>` MUST match `[a-z0-9][a-z0-9._-]*` and MUST be a canonical portable
  path component: at most 255 UTF-8 bytes, with no trailing `.`, reserved
  platform device name, separator, whitespace boundary, or control character.
- `local/<name>` identities are file-local and non-publishable. They are not a canonical path for any purpose other than naming the ephemeral module itself.
- Two inline mods with the same `local/<name>` in different files are distinct ephemeral modules.

#### 5.6.3 Content Rules

- The inline mod body follows all rules in Section 5.1 (`vo.mod`) and Section 5.2 (version constraints) except as narrowed below.
- Inline metadata MAY contain only `module`, `vo`, and `require` directives. Metadata tables are not permitted in an inline mod.
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
- a release manifest asset named `vo.release.json`
- a browser manifest asset named `vo.web.json`
- a source-package asset
- every declared target-specific artifact asset

The generated `vo.web.json` is both a fixed GitHub Release asset and a virtual
file at the module root inside the published source package. It is never
required to exist in the tagged repository tree.

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
- `source`: source-package asset metadata and canonical browser-readable file-set metadata:
  - `name`, `size`, and `digest` bind the compressed source-package asset bytes
  - `files_size` is the sum of UTF-8 content bytes for the files listed by `vo.web.json.source`
  - `files_digest` is the SHA-256 digest of the compact JSON array of those entries, sorted by path, with each entry encoded as `{path,size,digest}`
- `web_manifest`: fixed `vo.web.json` asset metadata:
  - `size` is the exact byte length of the generated `vo.web.json`
  - `digest` is the SHA-256 digest of those exact bytes
- `artifacts`: target-specific artifact metadata and digests; this list MAY be empty for pure-source modules but MUST include every published artifact required by the module's declared target-support contract

The release manifest is complete release metadata for CLI and native tooling.
It allows dependency resolution, native artifact selection, and integrity verification without interpreting arbitrary repository layout as package protocol.

Rules:

- `schema_version` MUST be an integer and MUST currently equal `1`.
- `module`, `version`, `vo`, `require`, `source`, and `artifacts` are authoritative published metadata.
- `require` MUST list only direct dependencies.
- `require` MUST be unique and sorted by module path.
- `artifacts` MUST be unique and sorted by `(kind, target, name)`.
- The published `artifacts` set MUST satisfy the target-support contract declared in `vo.mod`.
- Native artifacts, WASM artifacts, and generated JS glue artifacts are all release artifacts when declared.
- `module_root` MUST match the canonical module-path suffix inside the backing repository.
- `source.size` and `source.digest` MUST bind only the compressed archive asset.
- `source.files_size` and `source.files_digest` MUST bind the canonical UTF-8 file payload described by `vo.web.json.source`.
- `web_manifest.size` and `web_manifest.digest` MUST bind the exact fixed
  `vo.web.json` release-asset bytes. The `release_manifest` digest recorded in
  `vo.lock` therefore transitively binds all browser-only `web` and `extension`
  metadata.

### 6.3 Browser Manifest

Each published module version must provide a generated browser manifest named `vo.web.json`.
Release staging writes it as a fixed GitHub Release asset and includes the same
bytes as a virtual `vo.web.json` at the module root inside the source package.
Browser loading first checks the packaged module VFS for that file, then fetches
the `vo.web.json` asset from the GitHub Release for the exact module version.

`vo.web.json` is the authoritative browser consumption index for a module version.
It is generated from `vo.mod`, `vo.lock`, the committed source tree, and the staged release artifact contract.
It is not hand-authored.

The browser manifest must contain, at minimum:

- `schema_version`: manifest schema version, currently `1`
- `module`: canonical module path
- `version`: exact module version, or an explicit development snapshot identity for non-release project snapshots
- `commit`: immutable Git revision
- `module_root`: module root directory relative to repository root
- `vo`: toolchain constraint from `vo.mod`
- `require`: direct dependency constraints from `vo.mod`
- `source_digest`: digest of the browser-readable source set listed in `source`
- `source`: browser-readable source file list with path, size, and digest
- `web`: browser project metadata, when declared
- `extension`: browser-loadable extension metadata, when declared
- `artifacts`: browser-loadable WASM and JS artifacts, when declared

Rules:

- Browser runtimes MUST prefer `vo.web.json` over GitHub API tree enumeration.
- Browser runtimes SHOULD use a packaged module VFS copy when available. When
  no packaged copy is available, they MUST fetch the fixed `vo.web.json`
  GitHub Release asset for the exact module version.
- Browser runtimes MUST verify the raw `vo.web.json` byte length and SHA-256
  digest against `vo.release.json.web_manifest` before parsing or caching it.
- Browser runtimes MUST load files listed in `vo.web.json` from either the
  packaged module VFS or immutable raw Git content for the recorded commit.
- `source_digest` is the digest of the browser source payload described by `source`; it MUST equal `vo.release.json.source.files_digest` and is distinct from the archive digest in `vo.release.json.source.digest`.
- The sum of `source[*].size` MUST equal `vo.release.json.source.files_size`.
- Browser consumers MUST validate the `vo.web.json` identity, requirements, browser artifacts, source size, and source digest against the locked `vo.release.json` before installing files.
- `vo.web.json` MUST NOT list itself in `source`; including the manifest in its own source set would make the digest circular.
- `vo.release.json.web_manifest` binds `vo.web.json`, while `vo.web.json.source`
  excludes `vo.web.json`; release staging can therefore generate the browser
  manifest before the source archive and release manifest without a digest
  cycle.
- `vo.web.json` MUST NOT list native dynamic-library artifacts.
- `vo.web.json` MUST NOT contain local workspace overrides.
- `vo.web.json` MUST NOT contain unpublished local filesystem paths.
- `extension.wasm.kind`, when present, MUST use the exact protocol value
  `Standalone` or `Bindgen`.
- `vo.web.json` source entries MUST match the files committed at the recorded revision.
- `vo.web.json` artifact entries MUST match the browser-loadable artifacts declared in `vo.mod`.
- A browser runtime MUST fail if any fetched source file or browser artifact does not match the manifest size or digest.

### 6.4 Source Package

Each release must provide a canonical source-package asset.
The source package is the authoritative published source for the module version.

Format:

- The source package is a gzip-compressed tar archive (`.tar.gz`).
- The exact asset name is declared by `vo.release.json`; the asset name itself is not part of module identity.
- The archive must unpack into a single top-level directory.
- The archive contains the contents of the module root only, not the whole repository, unless the module root is the repository root.

Content rules:

- The source package must include `vo.mod` at the archive root.
- If present in the module, it must include `vo.lock`.
- It must include `vo.web.json`.
- The source package must include all `.vo` files required to build the module.
- The source package must include all source include paths declared by `vo.mod`.
- The source package must not include build artifacts, caches, or version-control metadata.
- The source package digest must match both the release manifest and `vo.lock`.
- A direct file-set installation must recompute and match `source.files_size` and `source.files_digest` before mutating the module cache.
- The `module` line in the packaged `vo.mod` must match the `module` field in `vo.release.json`.
- The packaged `vo.mod` `vo` line and `require` set MUST match the release manifest.
- The packaged `vo.mod` extension, web, and native target declarations MUST be consistent with `vo.release.json` and `vo.web.json`.
- If the source package contains a dependency-local `vo.lock`, consumers MUST ignore it when using this module as a dependency.

### 6.5 Target-Specific Artifacts

A release may provide target-specific binary artifacts.
If a module's published extension metadata declares support for a target-specific runtime artifact, the corresponding published artifacts are required for that target.
Examples include:

- prebuilt native extension libraries
- WASM extension binaries
- generated bridge files required by a target runtime

Rules:

- Target-specific artifacts must be listed in `vo.release.json`.
- An artifact's protocol identity is the complete `(kind, target, name)` tuple. `name` is its logical filename inside the module cache; two different identities MAY use the same logical filename.
- The flat GitHub Release asset name for an artifact MUST be `vo-artifact-v1-<hex>`, where `<hex>` is the lowercase SHA-256 digest of the UTF-8 byte sequence `vo-artifact-asset-v1\0<kind>\0<target>\0<name>` and `\0` denotes one zero byte.
- Within a materialized module entry, an artifact MUST be stored at `artifacts/<kind>/<target>/<name>`. The module-cache root remains implementation-defined.
- Artifact targets MUST use canonical target identifiers. For Rust-backed native binaries, the identifier MUST be the Rust target triple. For WASM artifacts, the identifier MUST be `wasm32-unknown-unknown`.
- A module version MAY support only a subset of targets. Unsupported targets simply do not appear in the declared target-support set.
- If `vo.mod` declares support for a target that requires a published binary artifact, `vo.release.json` MUST include that artifact set for the same target.
- Binary artifacts must be verified by size and digest before use.
- Binary artifacts do not change module identity or dependency resolution.
- A module remains source-defined even when binary artifacts are present.
- Native artifacts are consumed only by native tooling.
- WASM and JS glue artifacts are consumed by browser tooling only through `vo.web.json`.

### 6.6 Repository Mapping and Version Discovery

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

Resolution is a deterministic, complete backtracking search. Its normative
selection algorithm is:

1. Bound each untrusted registry listing to 10,000 entries before retaining
   it, sort it by descending semantic version, and collapse duplicate exact
   versions.
2. From the currently known unresolved modules, select the lexicographically
   smallest canonical module path.
3. Filter that module's versions by the module-path major rule, every active
   dependency constraint, release-manifest identity, and root toolchain-range
   coverage.
4. Visit the remaining candidates in descending semantic-version order. For a
   targeted update, a non-target module's existing locked version is moved to
   the front when it remains valid; the target keeps descending version order.
5. Depth-first search that candidate order, undoing the candidate's selected
   module and newly introduced constraints on failure. Continue until a full
   graph satisfies every edge, or until every branch has failed.

This algorithm defines the answer directly. It does not claim a globally
maximal final version vector: a later-discovered transitive module may make
another complete vector possible. Network order, release timestamps, cache
order, hash-map iteration order, and asynchronous completion order MUST NOT
affect the result.

Every solve uses one frozen registry snapshot. The first normalized version
listing for a module and the first raw manifest result for a `(module,
version)` pair, including an error, are retained for that solve. A key MUST be
read from the registry at most once. All typed manifest fields and the lock's
manifest digest derive from those exact retained bytes.

A listed release with malformed UTF-8, invalid syntax, or inconsistent release
identity is a definitively invalid release and MAY be skipped in favor of the
next candidate. Registry, network, filesystem, and global-budget failures leave
the candidate set unknown and MUST terminate the solve; they MUST NOT cause a
silent downgrade. A toolchain-mismatch error is reported only when every
successfully validated satisfying release is toolchain-incompatible and no
other candidate error remains.

The synchronous and asynchronous solvers MUST enforce identical checked
aggregate limits and return a structured resolution-limit error when exceeded:

- 10,000 selected non-root modules;
- 100,000 root plus transitive dependency edges;
- 100,000 target artifacts across the selected graph;
- 100,000 normalized candidates retained across the solve;
- 256 MiB of raw release-manifest bytes fetched and processed across the solve,
  charged before parsing so malformed releases cannot bypass the aggregate budget;
- 100,000 candidate decisions; and
- 100,000 backtracks.

The implementation MUST support the full selected-module limit without relying
on the native call stack or retaining a complete cloned graph for every search
level.

If no complete branch satisfies all constraints, resolution fails. Unsuffixed
module paths MUST NOT resolve to `v2+` releases. Suffixed module paths such as
`.../v2` MUST resolve only to releases whose semantic major version is `2`.

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

- no implicit version solving
- no lockfile mutation
- no silent dependency upgrades
- no fallback from failed integrity checks to unchecked source trees or raw repository fetches

Current public CLI compile/run/check/test entry points perform an
auto-materialization preflight before the frozen compile. That preflight may
download modules and artifacts that are already pinned by `vo.lock`, and
single-file inline modules with `require` entries may be resolved into an
ephemeral cache before compilation. The compile itself still uses the resolved
graph and must not rewrite the authored project graph.

### 8.2 Missing Lock or Missing Artifacts

If a build requires external modules and `vo.lock` is missing, the build fails and instructs the user to run a module-management command.

If `vo.lock` exists but required artifacts are missing from cache, low-level
frozen compile paths fail. Public CLI entry points that use auto-install may
first try to materialize the locked artifacts; if that preflight fails, the
build fails.

An active workspace override supplies local source authority for its module.
When every direct external requirement is covered by an active local override,
the frozen build MAY proceed without `vo.lock`. If any direct external
requirement remains uncovered, `vo.lock` is required. When a lock exists, it
MUST still describe and validate the complete root dependency graph, including
modules whose source is selected through workspace overrides.

### 8.3 Cache Model

The cache is an implementation detail.
It is not the source of truth for dependency identity.

Rules:

- For registry-backed dependencies, the source of truth for builds is the root
  `vo.lock`. An all-local workspace replacement graph may omit it as described
  in §8.2.
- Cache entries SHOULD be keyed by verified module path, exact version, and content identity.
- A cached source tree or artifact is valid only when it matches the exact locked version and the exact locked digest values.
- The toolchain MAY use a global cache, a project-local cache, or both.
- Cache-root layout is not part of the module identity contract. Within a materialized module entry, target artifacts use the canonical relative path `artifacts/<kind>/<target>/<name>` so complete artifact identities cannot overwrite one another.
- If a cached module or artifact fails validation against `vo.lock`, the implementation MUST treat it as missing.
- Cache installation transactions and active cache readers MUST hold a shared cache lease for their complete mutation or read lifetime. Cache cleanup MUST hold the corresponding exclusive lease and wait for shared leases to end before removing cache entries or abandoned staging data. The lease MUST use an operating-system lock whose ownership is released automatically when a process exits or fails. Every transaction and identity lock derived from a shared lease MUST retain that lease for its full lifetime. Every lease and derived transaction MUST retain capabilities for the exact `.vo-staging` directory and its non-linked regular `.lock` file, and MUST revalidate both names against those capabilities at every mutation or cleanup boundary. Replacing either pathname invalidates the lease authority and all subsequent mutations MUST fail closed.
- A cache root MUST contain the exact regular file `.vo-cache-owner` with content `volang-module-cache-v1\n`, link count one, and no symbolic-link traversal. A shared lease MAY create this marker with exclusive no-follow creation when the opened root is empty. It MAY also migrate a pre-marker cache only when the same opened root capability authenticates at least one complete current-format installed generation and every root entry is recognized. Each installed generation used by migration MUST have a canonical module cache key and exact version directory, a matching canonical `.vo-version`, a valid `.vo-source-digest`, a valid current `vo.release.json`, and a valid `vo.mod`; the release module, version, toolchain constraint, sorted requirements, and `source.digest` MUST agree with the directory identity, `vo.mod`, and source marker. The only additional migration entries are the exact legacy sidecars `.cargo-target`, `.tmp`, and `github.com` as real directories and `vo.sum` as a non-linked regular file; sidecars alone never prove cache ownership. Unknown entries, type mismatches, symbolic links, special files, malformed or legacy release schemas, and metadata mismatches MUST reject migration with an explicit upgrade-or-clean diagnostic. Migration MUST scan and revalidate through the same opened root capability, compare the exact root entry list again while holding the new marker's exclusive initialization lock, and roll back the marker without creating staging scaffolding if any entry or authenticated metadata changes. A successful migration MUST flush both the marker and its parent directory. An exclusive cleanup lease MUST require the pre-existing exact marker before creating or removing anything, MUST revalidate the held marker identity and content before destructive operations, MUST remove authenticated legacy sidecars through held directory capabilities without following nested symbolic links, and MUST never remove the owner marker. The filesystem root and the current working directory are forbidden cache roots.
- On platforms that support cache mutation, lock scaffolding and every transaction phase -- staging creation, directory and file creation, writes, verification, publication, and drop cleanup -- MUST remain bound to the same opened cache-root directory capability. Re-resolving the cache-root pathname MUST NOT redirect any transaction operation to a replacement tree. Every traversed component MUST be a real directory with its exact portable spelling; symbolic links and portable spelling aliases are hard errors.
- A platform MAY normalize a fixed, explicitly enumerated operating-system-owned path alias (for example macOS `/var` to `/private/var`) before opening the root capability. It MUST walk from the normalized trusted filesystem root with no-follow component operations and MUST NOT canonicalize or follow arbitrary user-controlled symbolic links.
- A cache lease or transaction MUST revalidate that its opened cache-root capability still names the configured cache-root path at mutation boundaries. If the configured path is rebound before the atomic rename, publication MUST fail without moving the staged entry, and no operation may create, remove, or publish data in the replacement tree. Capability-relative cleanup MAY still remove transaction-owned staging data from the detached original tree, but it MUST report the path-identity loss to an explicit caller.
- Cache cleanup MUST walk descendants iteratively through held directory capabilities without following symbolic links. One cleanup tree MUST reject depth beyond 256 directories or more than 100,000 total descendant entries before exceeding those budgets; malformed or adversarial staging data must produce a bounded error instead of recursive stack growth or unbounded work.
- Publication MUST use an atomic no-replace operation relative to the held source and destination directory capabilities. It MUST hold the source file or directory descriptor, revalidate the source name against that descriptor immediately before rename, and validate the committed destination name against the same descriptor immediately afterward. The held destination-parent capability MUST resolve to the exact configured relative path from the held cache root immediately before rename and both before and after the durability flush; the committed destination leaf and cache owner marker MUST likewise be revalidated before and after that flush. A post-rename identity mismatch is publication-committed/location-unconfirmed and MUST NOT be reported as success. A destination collision MUST preserve the existing entry and the staged transaction. Before publication, staged file contents and directories MUST be durably flushed. After publication, every affected parent directory MUST be flushed before durable success is reported; failure to flush one parent MUST NOT suppress the durability attempt for another affected parent.
- If the atomic no-replace, descriptor-relative traversal, no-follow validation, or durable file and directory flush primitives required above are unavailable, the implementation MUST fail closed before its first cache mutation.
- A successful atomic rename commits the destination namespace immediately. If a later parent-directory flush fails, the implementation MUST report a distinct publication-committed/durability-unconfirmed error, mark the transaction as published, and MUST NOT remove the destination during cleanup. The call that observed this failure MUST propagate that status even when the destination is immediately readable and valid; it MUST NOT reinterpret the failure as a concurrent no-replace collision. A retry MUST first validate the requested destination against the locked identity and digests; a complete valid destination is success, while an absent or invalid destination follows the normal missing-or-integrity-failure rules without assuming that the prior rename rolled back.
- If the configured cache-root path is rebound after the atomic rename, the implementation MUST preserve and flush the destination on the held original capability, report a distinct publication-committed/location-unconfirmed error, and MUST NOT write to or clean through the replacement path. The observing call MUST propagate that status even if the detached destination validates. A retry MUST open and validate the cache root currently named by the configured path; a valid destination there is success, while a missing destination may be installed by a new transaction under the new lease. The detached committed copy is never evidence that the current configured cache contains the entry.
- The reserved `ephemeral/<sha256>/` namespace is governed by the same lease, owner-marker, capability, and cleanup rules. A cache hit MUST read exactly `vo.mod` and `vo.lock` through one opened entry-directory capability and validate root consistency plus complete lock-graph reachability and edge constraints. The pair MUST be staged and published as one directory. Repair of an existing malformed directory MUST use an atomic directory exchange so readers holding the prior directory capability see one complete generation; in-place or two-file replacement is forbidden. The displaced generation MUST remain in staging until the exchanged identities, cache-root location, and affected parent-directory durability are all confirmed; an unconfirmed exchange MUST NOT delete that recovery evidence. Exclusive cache cleanup MAY remove the reserved `ephemeral` tree without counting it as an installed module version, while preserving the cache owner marker.

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
- File-name suffixes have no language-level filtering effect; `_test.vo` is an ordinary `.vo` file.
- A package named `main` is an executable package and cannot be imported.
- The default local import name is the imported package's declared package name.

### 9.4 Internal Packages

Vo supports Go-style internal visibility.

Rule:

- If an import path contains an `internal` boundary, that package may be
  imported only by packages whose canonical import path shares the parent
  prefix before that boundary. When a path contains multiple `internal`
  boundaries, the deepest boundary is authoritative.

This rule is evaluated on canonical import paths. Workspace overrides do not bypass it.

Example:

- internal package: `github.com/acme/app/internal/cache`
- allowed importer: `github.com/acme/app/cmd/tool`
- disallowed importer: `github.com/other/project/tool`

### 9.5 Build Constraints

This language version has no source-file build-constraint syntax. Every `.vo`
file in a selected package participates in a normal build. A leading comment
such as `//go:build` or `//vo:build` is an ordinary comment and has no filtering
effect.

Target-specific native or WASM artifacts use the manifest target tables in
Sections 5 and 8. Source-level target variation must remain explicit in package
APIs until a future language version defines a portable constraint grammar.

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

- A build has at most one active `vo.work`, selected by the `VOWORK` rules in
  section 5.4.
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
- locked module `deps` equality with both fields of `vo.release.json.require[*]` (`module` and `constraint`)
- every root and transitive locked dependency edge accepts the selected target version
- every selected dependency is reachable from a root requirement, with no missing, extra, duplicate, or conflicting selected module
- every locked dependency toolchain constraint covers the root `vo` constraint
- locked module artifact equality with `vo.release.json.artifacts`
- packaged `vo.mod` extension, web, and native target declarations consistency with `vo.release.json.artifacts`
- packaged `vo.web.json` consistency with `vo.mod`, committed source files, and browser-loadable artifacts
- packaged `vo.mod` consistency with `vo.release.json`

Any mismatch is a hard error.

The implementation MUST fail immediately on integrity mismatch. It MUST NOT fall back to a weaker validation mode.

### 12.2 No Implicit Trust in Repository Trees

The module protocol is based on release metadata and verified artifacts.
A toolchain must not treat arbitrary repository trees, raw file URLs, or archive snapshots outside the declared release protocol as equivalent to a published module release.
In particular, browser-manifest fallback uses the version's fixed
`vo.web.json` GitHub Release asset; a same-named raw file at the tag is outside
that metadata-discovery path.

### 12.3 Root-Lock Authority

Only the root module's `vo.lock` is authoritative for a build.
Dependency-local lockfiles may be present in published source packages or local override directories, but they do not alter the consuming build's graph.

## 13. CLI Behavior

The toolchain must expose module-management behavior equivalent to the following commands.
The exact command spelling may differ, but the semantics are normative.

### 13.1 `vo init <module-path>`

Creates a new `vo.mod` for the current module. The generated `vo` constraint is
the compatible constraint rooted at the running CLI's semantic version (for
example, `vo ^0.1.1` when initialized by Vo 0.1.1).

### 13.2 `vo mod add <module-path>[@constraint]`

Adds or updates a direct dependency constraint in `vo.mod`.
This command refreshes `vo.lock`.

If `@constraint` is omitted, the command resolves the latest non-prerelease published version and writes the equivalent compatible constraint as the direct requirement. If only pre-release versions exist, the command requires an explicit constraint.

### 13.3 `vo mod update [module-path]`

Re-solves dependency constraints, usually selecting newer compatible versions.
Updates `vo.lock`.

With an explicit `module-path`, the command SHOULD preserve unrelated locked versions when they still satisfy all constraints. It MAY update any shared transitive dependency required to reach a consistent graph.

### 13.4 `vo mod sync`

Recomputes the full dependency graph from `vo.mod`. When the module declares
external requirements, it verifies registry metadata and writes a fresh
`vo.lock` containing the full published artifact set for every locked module.
When the module declares no external requirements, the canonical result omits
`vo.lock` and the command removes any stale lock file.

### 13.5 `vo mod download`

Fetches artifacts already pinned by `vo.lock` into cache without changing the resolved graph.

This command MAY materialize source packages and target-specific artifacts, but it MUST NOT re-solve dependencies or rewrite `vo.lock`.
For a module with no external requirements and no `vo.lock`, the command succeeds
as an explicit no-op. A missing lock remains an error whenever `vo.mod` declares
an external requirement. An existing malformed lock is always an error.

### 13.6 `vo mod verify`

Verifies root `vo.mod` and `vo.lock` consistency and verifies that any already-materialized cached source or artifacts still match the lockfile.
For a module with no external requirements, an absent lock is the canonical
verified state. If a lock exists, it is parsed and validated even for an empty
graph; malformed data cannot be hidden by removing all requirements.

### 13.7 `vo mod remove <module-path>`

Removes a direct dependency from `vo.mod`.
This command refreshes `vo.lock` to prune orphaned transitive dependencies.

### 13.8 `vo release stage`

Stages a module release from one exact Git commit and the declared artifact inputs.

Rules:

- It MUST generate `vo.release.json` and stage it as a GitHub Release asset.
- It MUST generate `vo.web.json`, stage it as a GitHub Release asset, and embed
  the same generated bytes in the source package.
- Its reported upload list MUST contain exactly `vo.release.json`,
  `vo.web.json`, the source package, and every declared artifact asset, in a
  deterministic order.
- The output path MUST be absent when staging begins and when the anchored
  temporary directory is created. Every existing path, including an empty
  directory, regular file, or symbolic link, MUST be rejected without mutation.
- The release commit MUST exist, use its canonical 40-character lowercase
  object ID, and equal `HEAD` both before source capture and immediately before
  output publication. The staged index and tracked working tree below the
  module root MUST be clean relative to that commit.
- It MUST locate the Git repository top level and require the module path's
  `module_root` to equal the stage directory's canonical repository-relative
  path. Root modules use `.`; nested modules use their exact portable subpath.
- `vo.mod`, every published source byte, and each archive mode MUST come from
  the selected commit tree. Only regular `100644` and `100755` Git blobs are
  publishable; symbolic links, submodules, and other tree entry kinds MUST be
  rejected. The source manifest and archive MUST consume the same bounded,
  immutable snapshot.
- It MUST build every output in a private `0700` temporary sibling whose name
  contains at least 128 bits of operating-system randomness. Generated assets
  MUST use the exact recorded read-only mode and have exactly one hard link
  after creation. The implementation MUST revalidate the bounded exact entry
  set plus every asset's file identity, size, mode, link count, and digest while
  sealing and again immediately before publication. The entry-set bound is the
  maximum declared artifact count plus the three fixed release assets.
- Publication MUST remain anchored to one opened parent directory. The
  implementation MUST verify that the configured parent path still names the
  same filesystem object before and after sealing and publication, and that the
  temporary and final names identify the expected staged directory.
- The complete output MUST become visible with one atomic, no-replace directory
  rename relative to the anchored parent. Implementations MUST durably flush
  each file, the sealed temporary directory, and the parent directory after the
  rename. A platform or filesystem that cannot provide these primitives MUST
  fail closed with `AtomicPublishUnsupported` without creating the final
  release output.
- An ordinary error before a successful rename MUST remove only files created
  by that staging attempt and MUST leave the final output absent. A process
  crash MAY leave a private `.vo-release-stage-*` sibling. Unknown matching
  directories MUST NOT be removed automatically because their ownership and
  liveness cannot be inferred from the name. After confirming that no release
  process is active, an operator MAY inspect and remove only abandoned matching
  directories owned by that operator.
- If the no-replace rename succeeds and a subsequent parent-path check or
  parent-directory flush fails, staging MUST return
  `PublishedButDurabilityUnconfirmed`. The caller MUST treat the outcome as
  already published, inspect the requested path and its parent, preserve any
  discovered output, and MUST NOT retry into the same destination until its
  location and durability have been resolved.
- Source archive entries MUST be ordered by the UTF-8 bytes of their canonical
  portable module-relative paths so archive bytes are host-independent.
- It MUST resolve every declared include path against the selected commit tree,
  where it identifies either one regular blob or a non-empty directory prefix;
  directory expansion MUST NOT depend on the live working tree.
- Every `.vo` file and every explicitly included file MUST be valid UTF-8.
  Other binary files MAY remain in the full source archive, but MUST NOT enter
  `vo.web.json.source` or the installed module source cache.
- It MUST preserve the exact committed bytes of source files, including
  `Cargo.toml`; staging MUST NOT rewrite Cargo patch tables or compute metadata
  from bytes different from those placed in the archive.
- It MUST verify that every declared browser-loadable artifact byte-matches the
  regular blob at its declared path in the selected commit tree.
- It MUST verify that every declared native target artifact exists in the staged release inputs.
- It MUST fail if generated release metadata does not match the declared `vo.mod` contract.
- Before publication it MUST parse the written source archive through the
  module installer, compare the extracted UTF-8 source set and embedded
  `vo.web.json` byte-for-byte with the immutable snapshot, and pass installed
  module cache validation against the generated release metadata.
- It MUST NOT read `vo.work`.

### 13.9 `vo build`, `vo check`, `vo test`, `vo run`

Use the exact graph pinned in `vo.lock`.
Current public CLI entry points may materialize already-locked modules and
artifacts before compiling, and inline single-file modules with `require`
entries may resolve into an ephemeral cache. They must not mutate `vo.mod` or
`vo.lock`, re-solve an existing project graph, or silently upgrade locked
dependencies.

## 14. Not Supported by Design

The following are not part of the Vo module system:

- dependency aliases such as `@"name"`
- project-relative package imports
- filesystem-absolute package imports
- `std/...` import prefixes
- published `replace` directives inside `vo.mod`
- extension, web, or native publication metadata outside `vo.mod`
- multiple exact versions of the same canonical module path in one build graph
- dependency-local lockfiles influencing a consuming build's dependency graph
- source-building published dependency native artifacts during frozen builds
- implicit registry access during build commands
- interpreting arbitrary GitHub repository layout as the package protocol
- browser module loading that requires GitHub API access when `vo.web.json` is available

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
  declared by: vo.mod [extension.native]
```

## 16. Related Specifications

- `repository-layout.md`
- `native-ffi.md`
