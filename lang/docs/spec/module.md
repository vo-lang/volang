# Volang Module Protocol

Status: canonical

This document defines module identity, dependency resolution, workspaces,
published releases, and module lifecycle commands. The words **MUST**, **MUST
NOT**, **SHOULD**, and **MAY** are normative.

## 1. Protocol model

Volang separates authored intent, selected versions, published metadata, and
installed bytes:

| File | Owner | Purpose |
|---|---|---|
| `vo.mod` | author | module identity, toolchain constraint, direct dependencies, and public metadata |
| `vo.lock` v3 | toolchain | exact published dependency graph |
| `vo.work` v1 | developer | selected local source graph for workspace members |
| `vo.release.json` v2 | publisher | authenticated release index |
| `vo.package.json` v1 | publisher | exact source-package file closure |

A build has one effective graph authority. It is the root `vo.lock`, the
selected `vo.work` plus its complete reachable local member closure, or the
typed empty graph. A dependency's own lock file has no authority in the
consuming graph. Version constraints remain authored publication intent even
when an all-local workspace supplies the effective source graph.

All protocol files use strict schemas. Unknown fields are errors. Generated
files have deterministic ordering and MUST round-trip through their canonical
renderer.

### 1.1 Canonical JSON encoding

`vo.release.json` and `vo.package.json` use one byte-level JSON encoding:

- The document is UTF-8 without a BOM. Every structural line ends with LF,
  including the final line. CRLF, a missing final LF, blank lines, and extra
  whitespace are invalid.
- Indentation is two ASCII spaces per nesting level, a colon is followed by
  one ASCII space, and commas end the preceding member or array-element line.
  Non-empty objects and arrays use the multiline layouts shown in this
  specification. An empty array is written as `[]` on its member line.
- Object fields have these fixed orders: release root
  `schema_version, module, version, commit, vo, dependencies, source, package,
  artifacts`; dependency `module, constraint`; source `name, size, digest`;
  package binding `size, digest`; artifact
  `kind, target, name, size, digest`; package-manifest root
  `schema_version, files`; package file `path, mode, size, digest`.
- Ordering compares the canonical strings' raw UTF-8 bytes lexicographically,
  with no locale or UTF-16 code-unit ordering. Dependencies use module path;
  artifacts use the tuple `(kind, target, name)` field by field; package files
  use path. Module and artifact identity fields currently use ASCII, and the
  same byte rule applies to them.
- A string encodes `"` as `\"` and `\` as `\\`. U+0008, U+0009, U+000A,
  U+000C, and U+000D use `\b`, `\t`, `\n`, `\f`, and `\r`. Every other scalar
  in U+0000 through U+001F uses a lowercase four-digit escape `\u00xx`.
  Every remaining Unicode scalar is emitted directly as UTF-8; `/`, U+2028,
  and U+2029 stay unescaped.
- Unsigned integers use their shortest ASCII decimal spelling, with no sign
  or leading zeroes.

Any alternate JSON spelling is invalid even when a general JSON parser would
produce the same value.

## 2. Identity, imports, and versions

### 2.1 Module paths

A published module path has this form:

```text
github.com/<owner>/<repository>[/<subdirectory>...][/vN]
```

The `/vN` suffix is required for major versions 2 and above and is invalid for
major versions 0 and 1. It is recognized only as a segment after the repository;
a repository whose name is `vN` remains an ordinary unsuffixed module root. An
unsuffixed path accepts only major versions 0 and 1.

Every subdirectory segment is also a Git tag path component. It MUST NOT
contain `..` or end in `.lock`. Repository names do not appear in the release
tag and therefore retain those spellings. The remaining Git-ref restrictions
follow from the canonical lowercase portable-component grammar.

`local/<name>` is reserved for single-file inline modules. It MUST NOT appear
in imports, dependency tables, workspaces, locked dependency nodes, ordinary
on-disk project locks, or published metadata.

External imports are canonical, versionless package paths:

```vo
import "github.com/acme/graphics/draw"
```

Relative imports, absolute imports, `@version` suffixes, and host names other
than `github.com` are invalid. Standard-library imports use their bare package
path, such as `fmt`; the `std/` prefix is invalid.

### 2.2 Versions and constraints

Protocol values use bare semantic versions:

```text
1.2.3
1.2.3-rc.1
```

The `v` prefix is excluded from every protocol version value. Git tags retain
the prefix: version `1.2.3` is published under tag `v1.2.3`, or
`<module-root>/v1.2.3` for a module below the repository root.

Dependencies and toolchains accept three constraint forms:

| Form | Meaning |
|---|---|
| `1.2.3` | exact version |
| `^1.2.3` | compatible version range |
| `~1.2.3` | patch-compatible version range |

Prerelease identifiers use only ASCII lowercase letters, digits, and `-` so
distinct versions cannot collide in portable cache paths. Build metadata
(`+...`) is invalid. A version ending in `.lock` is invalid because the version
occupies the final Git tag component. A stable lower bound excludes prerelease
versions. A prerelease lower bound can select prereleases with the same
major/minor/patch core and the later stable version.

## 3. `vo.mod`

`vo.mod` is UTF-8 TOML. A minimal project is:

```toml
module = "github.com/acme/app"
vo = "^0.1.0"
```

A complete shape is:

```toml
module = "github.com/acme/app"
vo = "^0.1.0"

[dependencies]
"github.com/acme/graphics" = "^1.4.0"
"github.com/acme/http" = "1.3.1"

[web]
entry = "cmd/web/main.vo"

[extension]
name = "renderer"

[extension.native]
library = "vo_renderer"
targets = ["aarch64-apple-darwin", "x86_64-unknown-linux-gnu"]

[extension.wasm]
kind = "bindgen"
wasm = "renderer_bg.wasm"
js = "renderer.js"

[extension.web]
entry = "createRenderer"
capabilities = ["canvas"]

[extension.web.js]
renderer = "js/renderer.js"

[build.native]
kind = "cargo"
manifest = "rust/Cargo.toml"
package = "renderer-ext"

[build.wasm]
wasm = "web/pkg/renderer_bg.wasm"
js = "web/pkg/renderer.js"
```

### 3.1 Root fields

- `module` is required and contains the canonical module identity.
- `vo` is required and constrains the active Volang toolchain.
- `[dependencies]` maps each direct module path to one constraint. Entries are
  unique, cannot name the root module, and are canonically sorted by path.
- `[web].entry` selects an optional browser project entry. It must be a
  normalized portable path relative to the module root, name a `.vo` file, and
  exist in the authenticated source closure.

Release staging includes every Git-tracked regular blob below the module root.
A nested tracked `vo.mod` starts an independent module boundary, so the parent
closure excludes that complete subtree. Root project/release control files are
handled by the fixed rules in §7.3; directory names and filename extensions do
not otherwise filter tracked source data.

The source code imports package paths. `[dependencies]` declares the owning
modules. One direct module can own many imported packages.

### 3.2 Public extension contract

`[extension]` and its child tables describe what consumers can load:

- `[extension].name` is the stable extension owner name.
- `[extension.native].targets` lists supported Rust target triples.
- `[extension.native].library` optionally selects the portable logical library
  stem. It defaults to `[extension].name`. The published filename is derived
  from the stem and target (`lib<stem>.dylib`, `lib<stem>.so`, or
  `<stem>.dll`); authors do not write a platform filename here.
- `[extension.wasm]` declares a `standalone` or `bindgen` WASM artifact. `wasm`
  and optional `js` are logical published filenames. A bindgen extension
  requires `js`; a standalone extension forbids it.
- `[extension.web]` declares an optional runtime entry and capabilities.
- `[extension.web.js]` maps runtime module names to source paths. Those paths
  must be Git-tracked files in the authenticated source closure and cannot
  cross a nested module boundary.

Every public native/WASM artifact must appear exactly once in
`vo.release.json`. A module version may declare at most 997 artifacts: GitHub
allows 1000 assets per Release and the protocol reserves three for
`vo.release.json`, `vo.package.json`, and the source archive. Unsupported
targets fail explicitly. Each declared artifact must contain 1 through
268,435,456 bytes (256 MiB); publication, installation, cache validation, and
execution apply this same inclusive ceiling before allocating or loading its
payload.

The public library stem is independent of the Cargo package and Cargo library
target. `[build.native]` selects local Cargo inputs and does not enter the
public extension or release-artifact identity. The complete raw `vo.mod`,
including `[build.*]`, remains part of the authenticated source identity.

The resolved canonical module path is the artifact's extern-owner identity.
Every extension backend selects the longest loaded canonical module owner on
an exact segment boundary. Descendant package segments are case-sensitive and
may contain portable Unicode when they satisfy the canonical package rules.
Native and browser catalogs therefore make the same owner choice regardless
of load order.

Browser exports use `__vo_ext_` followed by the lowercase hexadecimal form of
every UTF-8 byte in the complete canonical encoded extern name.
Decoded-function, full-wire-name, flattened, and textual-prefix aliases never
define export identity. If the deepest selected owner lacks that exact export,
the runtime MUST NOT fall back to a parent owner.

Every final browser artifact exports `vo_ext_protocol_version()` and must
return browser protocol version `3`. Loading identical bytes for a live owner
is idempotent; a transport URL does not define artifact identity, and explicit
disposal is required before intentional replacement. Concurrent identical
loads join one transaction. Setup may prepare an instance, but a prepared
artifact remains outside active dispatch maps until synchronous commit
succeeds.

Extern resolution freezes the selected `(owner, generation)` and the bridge
validates the frozen binding before and after every JavaScript export,
including replay. The complete wire encoding, transaction, cancellation,
disposal, and replay rules are defined by the browser WASM protocol v3 in
`native-ffi.md` section 6.

### 3.3 Local build adapters

`[build.*]` maps local files and build tools to the public extension contract.
These fields never enter release metadata or consumer identity.

- `[build.native] kind = "cargo"` requires `manifest` and accepts an optional
  Cargo `package` selector.
- `[build.native] kind = "prebuilt"` requires a module-relative `path`.
- `[build.wasm]` provides module-relative `wasm` and, for bindgen, `js` inputs.

A native build adapter requires `[extension.native]`; a WASM build adapter
requires `[extension.wasm]`. Publication validates the produced bytes against
the public artifact set.

A Cargo `manifest` must name `Cargo.toml` below a dedicated top-level module
directory. Its first path component is the module's reserved opaque native
root; the manifest may be nested at any depth below that root. `.git`,
`.volang`, `.vo-cache`, `node_modules`, and `target` cannot be selected as this
root under the portable Unicode case-folding rules, so aliases such as `.GIT`
and `Target` are rejected on every host. Vo source and module-control files
(`*.vo`, `vo.mod`, `vo.lock`, and `vo.work`) are forbidden throughout the
native source-input portion of the root. Generated and declared cache subtrees
remain opaque and are not enumerated for this check. The base language snapshot never
enumerates this tree, so unrelated Rust inputs, links, and vendored content
cannot affect a compilation that does not reach the extension.

Native Cargo adapters are prepared only for extension-owning packages reached
by the analyzed import graph. An unused declared dependency or workspace member
still participates in module and workspace authority, while its native tools
and Cargo graph are not required by that compilation. Once an extension is
reached, its complete producer fingerprint and stable-input rules apply before
the artifact can enter the compiled output or compile cache.
Prebuilt adapter bytes follow the same reachability boundary: their exact path
is opened and authenticated only after the owning extension is reached.

## 4. Inline modules

A standalone `.vo` file can embed the restricted `vo.mod` schema in a reserved
leading-comment block:

```vo
/*vo:mod
module = "local/demo"
vo = "^0.1.0"
*/

package main
```

The block body is TOML and accepts only `module` and `vo`. The module identity
must use `local/<name>`. Dependencies, publication, web, extension, and build
sections are invalid. A single file that needs an external import must be
promoted to a `vo.mod` project with a committed `vo.lock`.

The sentinel must occur in the complete leading comment region before the
first source token. Copyright headers and ordinary comments may precede it.
That leading region may contain only one reserved `/*vo:mod` block; identical
text after the first source token has ordinary source-comment or string
semantics. A file inside an existing `vo.mod` project cannot declare an inline
module. Inline modules ignore `vo.work`.

Inline modules compile directly and create no generated `vo.mod` or `vo.lock`.

## 5. `vo.lock` v3

Projects with a registry-backed external edge commit `vo.lock`. A build may
omit the lock only when the selected workspace supplies every reachable
external module and every reachable dependency edge from local member
manifests. Projects with an empty external graph also omit it. The file is
generated TOML and must not be edited by hand. A v3 lock with no `[[module]]`
entry is invalid even when its root fields are otherwise well formed.

```toml
version = 3

[root]
module = "github.com/acme/app"
vo = "^0.1.0"

[[module]]
path = "github.com/acme/base"
version = "1.1.0"
vo = "^0.1.0"
release = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
dependencies = []

[[module]]
path = "github.com/acme/graphics"
version = "1.4.2"
vo = "^0.1.0"
release = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
dependencies = [
  { module = "github.com/acme/base", constraint = "^1.0.0" },
]
```

Each `[[module]]` contains only:

- `path`: canonical module path;
- `version`: selected bare semantic version;
- `vo`: selected release's toolchain constraint;
- `release`: SHA-256 digest of the exact raw `vo.release.json` bytes;
- `dependencies`: direct dependency edges copied from that release.

The root identity and toolchain constraint must equal `vo.mod`. The module set
must contain every reachable external dependency exactly once and no
unreachable entry. Every edge must target a selected version satisfying its
constraint. Every selected release must support the complete root toolchain
range.

Source archive names, commits, package digests, file lists, and artifacts live
in the authenticated release/package manifests. Duplicating those fields in
the lock would create competing authorities.

## 6. `vo.work` v1

`vo.work` is local TOML with one member list:

```toml
version = 1
members = [".", "../graphics", "../http"]
```

Every member path resolves relative to the workspace file and must identify a
real directory containing `vo.mod`. The member identity is always read from
that manifest. Paths use canonical `/` separators: `.` and leading `../`
components are allowed, while absolute paths, `./name`, backslashes, trailing
slashes, and `..` after a named component are invalid. Paths that collide under
portable case-folding are also invalid. Duplicate resolved directories,
duplicate identities, and symbolic-link substitution are errors. `.` may list
the workspace-file directory explicitly, but the active root is always
implicit and is never selected as a dependency source. Any other member path
that declares the active root identity is rejected as a duplicate or identity
ambiguity.

With `VOWORK` unset, tooling selects the nearest ancestor `vo.work`.
`VOWORK=off` disables workspaces. Any other `VOWORK` value selects that file
explicitly, with relative values resolved from the root project.

A selected workspace has two valid authority modes:

1. With `vo.lock`, members select local source only for matching locked
   modules. Graph-external members are ignored, and every outgoing edge stays
   equal to the locked release edge.
2. Without `vo.lock`, the root direct dependencies and every dependency
   reachable from them must resolve to members of that exact selected
   `vo.work`. Member `vo.mod` files supply the edges, and source imports must
   be covered by those edges. The closure must contain no missing, open, or
   registry-backed edge.

Any incomplete local closure requires a complete v3 lock. Nested workspace
and lock files in a member are ignored. Workspace authority applies to
development builds and the default `graph`, `snapshot`, and `why` views.
Explicit `--declared` inspection, disabled-workspace operation, dependency
verification, and release verification require the registry graph and its
complete v3 lock.

## 7. Publication protocol

### 7.1 Immutable release

A published version is valid only when its Git tag resolves to the commit
recorded by `vo.release.json`, the GitHub REST release object reports
`immutable: true`, and the GitHub Release contains this exact asset set:

1. `vo.release.json`;
2. `vo.package.json`;
3. the fixed source archive `source.tar.gz`;
4. every declared artifact, using its canonical opaque release-asset name.

Every asset must report `state: "uploaded"`, its exact byte size, and a
canonical `sha256:<64 lowercase hex>` GitHub asset digest. The digest of
`vo.release.json` must equal SHA-256 of its downloaded raw bytes. The package,
source, and artifact digests must equal their corresponding bindings inside
that authenticated release manifest. Missing, duplicate, additional,
non-uploaded, size-mismatched, or digest-mismatched assets invalidate the whole
release.

The source archive is always named `source.tar.gz` and contains the single
top-level directory `source/`. That directory contains the exact
files listed by `vo.package.json` plus the exact `vo.package.json` bytes at its
root. Release staging reads committed Git blobs, rejects a dirty or moving
release commit, and publishes atomically into a fresh output directory. Every
artifact input is opened through anchored directory handles. A privileged
root-level operating-system alias such as macOS `/var` may be resolved once;
symbolic links in the final name or any later parent component are rejected
before bytes enter the staging closure.

For remote acceptance, source authority comes from the immutable
`source.tar.gz` asset, its release-manifest binding, and its embedded
`vo.package.json`. The recorded commit anchors tag and release provenance and
declares the producer's staging input. It does not provide a remote proof that
independently derives every source byte or mode from a Git tree.

### 7.2 `vo.release.json` v2

The release index has this shape:

```json
{
  "schema_version": 2,
  "module": "github.com/acme/graphics",
  "version": "1.4.2",
  "commit": "0123456789abcdef0123456789abcdef01234567",
  "vo": "^0.1.0",
  "dependencies": [
    {
      "module": "github.com/acme/base",
      "constraint": "^1.0.0"
    }
  ],
  "source": {
    "name": "source.tar.gz",
    "size": 12345,
    "digest": "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
  },
  "package": {
    "size": 456,
    "digest": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
  },
  "artifacts": []
}
```

Dependencies are unique and sorted by module path. Artifacts are unique and
sorted by `(kind, target, name)`; each also contains `size` and `digest`.
`commit` is the tag/release provenance anchor and the producer's immutable
staging-input declaration; it is not a remote per-file Git-tree proof.
`source` binds the fixed `source.tar.gz` compressed archive. `package` binds the exact standalone
`vo.package.json` asset. The raw `vo.release.json` and bound
`vo.package.json` assets are each limited to 16 MiB.

### 7.3 `vo.package.json` v1

The package manifest describes the complete authenticated raw-byte release
source closure. A conforming producer derives it from every Git-tracked
regular blob in the clean release commit within the current module boundary,
subject only to the fixed root control rules below.

```json
{
  "schema_version": 1,
  "files": [
    {
      "path": "src/draw.vo",
      "mode": "regular",
      "size": 120,
      "digest": "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    },
    {
      "path": "vo.mod",
      "mode": "regular",
      "size": 96,
      "digest": "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
    }
  ]
}
```

Entries are unique and sorted by their canonical paths' raw UTF-8 bytes as
defined in §1.1. `mode` is required and is either `regular` or `executable`;
it authenticates the installed owner-execute state while all other permission
bits remain toolchain-owned. The root `vo.mod` is required, non-empty, and
always has mode `regular`; `vo.mod` never carries execution semantics. Digests
cover raw bytes, so source closures can contain text and binary files.
At the package root, `vo.lock` and `vo.work` are excluded. Root `vo.sum`,
`vo.web.json`, `.vo-project.lock`, and `.vo-project.transaction` must be
removed before release. Root `artifacts`, `.vo-version`, `.vo-source-digest`,
`vo.release.json`, `vo.package.json`, and `source.tar.gz` are package-owned and
cannot enter the source closure. Those protocol-looking names are ordinary
source data below ordinary directories. Path traversal, symbolic links,
non-portable collisions, missing entries, and extra entries are rejected.

The root file is spelled exactly `vo.mod`. Any other root spelling equivalent
to `vo.mod` under portable case folding, and any nested file whose basename is
equivalent under the same rule, is rejected as an independent module boundary.

Every source-archive member is a regular tar entry. `regular` maps exactly to
tar mode `0644`; `executable` maps exactly to tar mode `0755`; the embedded
`vo.package.json` entry uses `0644`. Any other tar mode is invalid. Native Unix
cache installation writes these classes as exact private modes `0600` and
`0700`, respectively, and cache validation rechecks the authenticated owner-
execute state. Platforms and virtual filesystems without a POSIX executable
bit still authenticate the `mode` field and preserve it in package metadata.

Logical archive members are sorted by their package-relative path using the
UTF-8 byte ordering in §1.1; `vo.package.json` participates in that order.
There are no explicit directory members. Each logical member's wire path is
`source/` followed by its package-relative path. The relative path is limited
to 4,096 UTF-8 bytes, so the wire path is limited to 4,103 bytes.

A wire path of at most 100 bytes is stored directly in its GNU regular-file
header. A longer wire path has exactly one canonical GNU longname extension
immediately before its regular-file header:

- The extension header has type `L`, name `././@LongLink`, mode `0644`, UID 0,
  GID 0, mtime 0, and a declared size equal to the wire-path byte length plus
  one.
- Its data is the exact UTF-8 wire path followed by one terminal NUL, with no
  interior NUL. The declared extension data is therefore at most 4,104 bytes;
  tar block padding is zero and is outside that declared size.
- The following regular-file header's 100-byte name field contains the
  longest prefix of the wire path that fits without splitting a UTF-8 scalar.
  The extension supplies the authoritative complete path.

A GNU longname is invalid for a path that fits directly, when repeated,
dangling, separated from its file header, or paired with a different truncated
prefix. GNU longlink (`K`), PAX local or global headers (`x` or `g`), sparse
entries, links, and every other extension type are invalid. Longname headers
do not count toward the 100,000 logical-member limit; the raw stream is
separately limited to at most twice that many physical headers. Consumers
validate the extension size before reading its data and retain at most the
4,104-byte bounded payload.

Every unused byte in an entry's tar-block padding is zero. The final member is
followed by exactly two 512-byte zero blocks and no further decompressed data.
The canonical producer writes gzip mtime 0. Consumers require one complete
gzip member with a valid CRC32 and ISIZE and reject concatenated members or
trailing compressed bytes.

The compressed archive is limited to 64 MiB and the complete extracted tree,
including the embedded package manifest, is limited to 128 MiB. Each package
file is limited to 64 MiB. `vo.mod` and every `*.vo` file additionally obey the
16 MiB text-consumer limit. Declared sizes are validated before allocation and
must equal the extracted raw-byte lengths. An archive contains at most 100,000
entries including its embedded `vo.package.json`, so the manifest lists at most
99,999 source files.

The complete portable path closure has a separate 16 MiB path-key budget.
Implementations retain one spelling and one folded key per distinct component-
trie node. Protocol accounting charges each newly retained node by the UTF-8
byte length of its complete package-relative prefix, including `/` separators
and implicit directory nodes. A shared prefix is charged once. The producer,
native archive/cache consumers, browser installer, Quickplay generator, and
Studio validator apply this same incremental rule before retaining or copying
the new path.

### 7.4 Authentication chain

Remote consumers first verify the immutable GitHub release object, complete
asset inventory, per-asset SHA-256 digests, and tag commit. They then verify in
this order:

```text
vo.lock.release digest
  -> raw vo.release.json
     -> source archive digest
     -> raw vo.package.json digest
        -> every package file's raw bytes
     -> every target artifact's raw bytes
```

The package bytes embedded in the source archive must equal the separately
published package asset. The archive's extracted file set must equal the
package closure. Cache publication occurs only after the complete chain is
valid and uses atomic, no-follow filesystem operations.

There is no browser-specific module manifest. Remote browser consumers
authenticate and download `source.tar.gz`, then use the same archive and
embedded-package parser as native consumers. Native and browser transports
apply the same immutable GitHub release proof, exact asset inventory, package
closure, byte digests, and source modes.

## 8. Resolution and builds

The solver reads direct intent from root `vo.mod` and transitive intent from
candidate `vo.release.json` files. A selected graph must satisfy every version
constraint, module-major rule, and root toolchain-coverage rule.

Build-like commands consume one validated effective authority: the empty
graph, a complete v3 lock, or a selected all-local workspace closure. They do
not re-solve the graph, mutate `vo.mod`/`vo.lock`, or upgrade versions. Native
build paths may materialize missing bytes only for a locked graph. A cache hit
is accepted only after release, package, source, and required artifact
integrity are revalidated.

The native default cache root is `~/.vo/mod/v1`. The final `v1` component is
the cache-layout generation and carries the matching owner marker. A future
incompatible layout selects a fresh versioned leaf and leaves older data
untouched. `VO_MOD_CACHE` replaces the complete root exactly; it does not gain
an implicit version suffix and must be a non-empty absolute path. Any selected
root that is non-empty without the current owner marker fails closed and is
never adopted automatically. If the native tool cannot determine an absolute
user home directory, cache-root selection fails until an absolute
`VO_MOD_CACHE` is supplied.

An active workspace changes the effective source directory for matching locked
modules or supplies the complete lockless local graph. `vo mod graph`, `vo mod
snapshot`, and `vo mod why` expose that build authority by default. Their
`--declared` view disables workspace discovery and exposes authored intent plus
the selected registry graph, which requires a complete v3 lock for a non-empty
dependency graph.

## 9. CLI lifecycle

`vo init <module>` creates the minimal canonical `vo.mod` in an otherwise
uninitialized directory. It refuses to replace an existing `vo.mod` and
refuses an orphan `vo.lock`, so project creation cannot inherit an unrelated
selected graph.

Graph mutation and cache mutation have separate commands:

| Command | Effect |
|---|---|
| `vo mod add <module>[@constraint]` | update direct intent and the named selection; preserve unrelated valid selections |
| `vo mod update [module]` | update every selection, or update the named selection while preserving unrelated valid selections; a named target must remain in the resolved graph |
| `vo mod sync [path]` | repair the graph while preserving valid selections; write v3 lock, or remove any stale lock bytes when the authored graph is already empty |
| `vo mod remove <module>` | remove direct intent and preserve every remaining valid selection |
| `vo mod tidy [path]` | align direct intent with imports and preserve selections that remain valid |
| `vo mod fetch [path]` | authenticate and fill cache from the existing lock; never solve |
| `vo mod verify [path]` | read-only graph, lock, and cache integrity verification |
| `vo mod why <module> [--declared]` | read-only shortest dependency explanation for the effective graph by default |
| `vo mod graph [path] [--declared]` | read-only human view of the effective graph by default |
| `vo mod snapshot [path] [--declared]` | read-only canonical JSON graph export (schema v2: `empty`, `lock`, or `workspace` authority) of the effective graph by default |
| `vo cache clean` | explicitly remove every installed version from the active cache generation while retaining the owned root and protocol infrastructure |

When `vo mod add` omits the constraint, it selects the latest non-prerelease
release and writes the equivalent compatible constraint into `vo.mod`.

Existing lock versions are solve preferences, never constraints. `sync`,
`add`, `remove`, and `tidy` keep each prior version when it still satisfies the
new complete graph; conflicts advance only the nodes needed to obtain a valid
solution. `vo mod update <module>` drops that preference for the named module.
`vo mod update` without a module drops all prior preferences and selects the
highest valid complete solution. A structurally valid v3 lock can seed these
preferences even after dependency intent changes. Root identity or toolchain
drift discards the whole preference set, while malformed lock bytes remain an
error. A targeted update succeeds only when its named module remains in the
resolved graph; failure leaves both project files unchanged.

Commands that can retain or grow a graph parse and validate any existing
`vo.lock` before using or replacing it. This includes `add`, `remove`, `tidy`,
and targeted `update` when the authored graph is empty. Malformed bytes fail
the command and leave `vo.mod` and `vo.lock` unchanged. `vo mod sync` and
untargeted `vo mod update` are the two explicit cleanup paths: when the
authored graph is already empty, they remove stale lock bytes without treating
them as prior selections.

Dependency mutations may read registry release metadata during solving, but
they do not download source packages or artifacts. `fetch` never changes the
selected graph. Read-only commands create no project files and perform no
cache cleanup.

Release commands are separate:

- `vo release verify [path]` requires a clean HEAD and checks the committed
  module, the complete v3 lock when registry dependencies exist, the dependency
  graph, tracked source closure, and configured local build inputs without creating
  release output.
- `vo release stage [path] ...` generates the v2 release manifest, v1 package
  manifest, source archive, and the exact declared artifact set into a fresh
  staging directory. Staging performs the final artifact identity and byte
  validation.

## 10. Reproducibility rules

- Commit `vo.mod` and every required `vo.lock`; omit a lock only for an empty
  graph or an explicitly selected, complete all-local workspace closure.
- Treat the selected `vo.work`, every reachable member manifest, and every
  validated source import as one lockless workspace authority snapshot.
- Run release verification with workspace discovery disabled.
- Never derive published identity from local paths, cache layout, or build
  adapters.
- Never trust parsed metadata before its enclosing raw-byte digest has been
  verified.
- Reject ambiguous paths, duplicate identities, unsupported targets, stale
  dependency edges, incomplete package closures, and digest mismatches.
- Keep generated protocol files canonical and byte-stable.

Repository organization and tag placement are specified in
[`repository-layout.md`](./repository-layout.md). Single-file usage is covered
by [`module-inline-mod-tutorial.md`](./module-inline-mod-tutorial.md).
