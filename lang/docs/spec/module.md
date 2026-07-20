# Volang Module Protocol

Status: canonical

This document defines Volang module identity, authored intent, dependency
selection, local workspaces, published releases, materialization, and module
lifecycle commands. The words **MUST**, **MUST NOT**, **SHOULD**, and **MAY**
are normative.

## 1. Model and invariants

The protocol has six public concepts:

| Concept | Representation | Purpose |
|---|---|---|
| module identity | import path and `module` | stable logical ownership |
| intent | `vo.mod` | authored version, toolchain floor, dependencies, and public metadata |
| selection | `vo.lock` | the one exact dependency graph |
| workspace | `vo.work` | local module identity-to-directory mapping |
| release | `vo.release.json` | immutable published source and artifact descriptor |
| plan | `ProjectPlan` | one command's validated graph, sources, target, and purpose |

Every non-empty dependency graph has exactly one `vo.lock`. The lock may
select registry releases, workspace modules, or both. `vo.work` never derives
an executable graph during a build; it only supplies directories for modules
already selected by `vo.lock`.

Build-like commands are graph-frozen. `build`, `run`, `check`, and `test` MUST
NOT solve versions or modify `vo.mod`, `vo.lock`, or `vo.work`. They MAY fetch
only objects already selected by digest.

The same `ProjectPlan` and target-aware `MaterializationPlan` are consumed by
native, browser, Studio, and release adapters. Adapters MUST NOT
rediscover a workspace, reinterpret a lock, or select artifacts independently.

Protocol readers validate a strict typed schema but accept semantically
equivalent TOML or JSON encodings. Protocol writers emit deterministic,
canonically ordered text. Raw release descriptor bytes are content-addressed;
their digest, not a mandated whitespace spelling, defines release identity.

## 2. Identity and versions

### 2.1 Module identity

A public module identity is a lowercase, host-qualified path:

```text
<dns-host>/<segment>/<segment>...[/vN]
```

Examples:

```text
github.com/vo-lang/voplay
example.com/acme/render/v2
```

The identity is transport-neutral. A `github.com/...` identity does not require
consumers to call the GitHub API. Registry routing and mirrors never change
module identity.

Hosts and module segments use the canonical lowercase ASCII portable-component
grammar. Ports, queries, fragments, empty segments, `.` and `..` are invalid.
Major versions zero and one use an unsuffixed identity. Major versions two and
above require the final `/vN` segment. The complete ModuleId is the single-
version key, so distinct major identities may coexist in one graph.

`local/<name>` is reserved for unpublished project and workspace modules. A
local identity may enter a workspace-selected lock. It MUST NOT enter a
portable lock or a published release.

Imports are versionless package paths. An external package belongs to the
longest selected ModuleId on an exact segment boundary. Relative imports,
absolute imports, version suffixes, and ambiguous path spellings are invalid.

### 2.2 Module versions

Versions are bare semantic versions such as `1.2.3` or `1.2.3-rc.1`. A leading
`v`, build metadata, uppercase prerelease identifiers, and non-portable cache
spellings are invalid.

Dependency constraints have exactly three forms:

| Form | Meaning |
|---|---|
| `1.2.3` | exact version |
| `^1.2.3` | compatible version |
| `~1.2.3` | patch-compatible version |

A stable lower bound excludes prereleases. Prereleases require an explicit
prerelease lower bound.

### 2.3 Toolchain minimum

The `vo` field is one bare minimum toolchain version, not a version range.
Before 1.0, a compatibility epoch is one major/minor pair. From 1.0 onward, a
compatibility epoch is one major version. A compiler accepts a module only when
it supports the same epoch and is no older than the declared minimum.

The root's minimum MUST be at least the greatest minimum selected anywhere in
its dependency graph. Resolution reports the exact root minimum required when
this rule is violated.

## 3. `vo.mod`

`vo.mod` is authored TOML:

```toml
format = 1
module = "github.com/acme/app"
version = "0.2.0"
vo = "0.2.0"

[dependencies]
"github.com/acme/graphics" = "^1.4.0"
"github.com/acme/http" = "1.3.1"
```

The required root fields are:

- `format = 1`;
- `module`, the canonical ModuleId;
- `version`, the module's exact authored version;
- `vo`, the minimum compatible Volang toolchain.

`[dependencies]` maps direct ModuleIds to constraints. Dependencies are unique,
cannot name the root identity, and are rendered in ModuleId order.

The existing `[web]`, `[extension]`, and `[build]` namespaces remain the public
application, extension, and producer contracts. The public extension contract
participates in the module intent digest. Local producer paths and incidental
TOML formatting do not.

The typed module intent digest is:

```text
sha256(
  "vo-module-intent-v1",
  module,
  version,
  vo,
  sorted direct dependencies,
  canonical public web/extension contract
)
```

Unknown keys are errors.

## 4. `vo.lock`

Any root with a non-empty dependency table MUST have `vo.lock`. A dependency-
free root omits it.

```toml
format = 1
root = "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

[[module]]
path = "github.com/acme/graphics"
version = "1.4.2"
origin = "registry"
release = "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

[[module]]
path = "github.com/acme/render"
version = "1.5.0-dev.2"
origin = "workspace"
intent = "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
```

The root digest binds the exact typed root intent. Every module entry contains:

- `path`, the selected ModuleId;
- `version`, the exact selected version;
- `origin`, either `registry` or `workspace`;
- `release`, required only for a registry origin;
- `intent`, required only for a workspace origin.

A registry release descriptor supplies the selected module's dependency edges,
toolchain minimum, source object, and artifacts. A workspace member's typed
intent supplies the corresponding local descriptor. The lock does not copy
those facts.

Validation loads every exact descriptor, traverses from the root dependency
table, checks every constraint, verifies single-version identity, and requires
the reachable set to equal the complete lock node set. Registry descriptor
bytes must match `release`; workspace intent must match `intent`.

Malformed generated locks are never build authority. `vo mod sync` and
`vo work sync` may discard malformed prior selections with an explicit warning
and generate a fresh lock. Read-only commands return a structured lock error.

### 4.1 Portable and workspace locks

A lock whose nodes all have registry origin is portable. A lock containing any
workspace-origin node is workspace-bound.

Portable mode disables local source overlays and rejects every workspace-origin
node. Release verification, CI portability checks, and ordinary remote project
imports use portable mode.

## 5. `vo.work`

`vo.work` is a local module-directory catalog:

```toml
format = 1
members = ["BlockKart", "vogui", "vopack", "voplay"]
```

Each member path is relative to the workfile directory and names a directory
containing `vo.mod`. Paths may be `.` or normalized descendants. Absolute
paths, `..`, `./name`, backslashes, trailing separators, portable collisions,
duplicate real directories, and duplicate ModuleIds are invalid.

Cross-repository development places `vo.work` in the repositories' common
parent directory. Repository workspaces place it at a common monorepo root.

The active root MUST be an explicit member. Automatic discovery examines the
nearest ancestor `vo.work`; it selects that file only when the active root is
listed. Otherwise workspace use is off and discovery does not continue past
that boundary. An explicit workfile that omits the root is an error. Nested
workspaces never merge.

The core receives an explicit workspace selector and never reads environment
variables. CLI or host adapters may translate `VOWORK` into `auto`, `off`, or
an exact workfile before creating a `ProjectRequest`.

### 5.1 Workspace-origin node

A workspace-origin lock node requires a selected member with matching ModuleId,
version, and intent digest. Source edits do not change the lock. Changing any
intent field requires `vo work sync`.

A registry-origin node always uses authenticated registry source. To select a
local directory, run `vo work sync` so the node's origin changes explicitly.

Graph-external members are ignored.

## 6. Resolution

Only module mutation commands may list versions and solve a graph.

`vo mod sync` ignores workspace members and produces a portable, registry-only
lock. `vo work sync` resolves matching ModuleIds from the selected workspace
and all remaining modules from registries, producing one mixed lock when
needed.

Resolution follows these rules:

- one version per complete ModuleId;
- deterministic ModuleId and candidate ordering;
- stable releases before prereleases unless explicitly requested;
- prior valid selections are preferences, never extra constraints;
- targeted update drops the preference only for its target;
- untargeted update drops every preference;
- one command-scoped registry snapshot freezes every key and error;
- unavailable, authentication, and rate-limit failures abort instead of being
  misreported as unsatisfiable graphs;
- conflicts carry a minimal constraint chain.

Mutation uses a short transaction: capture exact `vo.mod`/`vo.lock` generation,
release the project lock, perform registry I/O and solving, reacquire the lock,
compare the generation, then atomically commit or report concurrent mutation.
Network I/O never occurs while holding the project mutation lock.

## 7. Registry protocol

Registry transport is asynchronous and shared by native and browser clients.
Its semantic operations are:

```text
list(ModuleId) -> [(Version, ReleaseDigest)]
release(ModuleId, Version, expected ReleaseDigest?) -> byte stream
blob(Digest) -> byte stream
```

Only resolution may call `list`. A build may request only an exact release
descriptor or blob already selected by digest.

Registry routing is independent of ModuleId. A router may select the default
Volang registry, a private registry, a mirror, a filesystem fixture, or a
GitHub-backed adapter. Mirrors affect availability only. Every client recomputes
size and digest and parses raw descriptor bytes centrally.

The registry is responsible for namespace ownership and immutable publication
of each `(ModuleId, Version)` pair. Yank status is mutable index metadata; an
existing lock remains installable. GitHub REST fields, tags, asset inventory,
and release immutability flags are adapter policies and never language wire
fields.

## 8. Published release

`vo.release.json` is strict JSON:

```json
{
  "format": 1,
  "module": "github.com/acme/graphics",
  "version": "1.4.2",
  "vo": "0.2.0",
  "intent": "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
  "dependencies": [
    { "module": "github.com/acme/base", "constraint": "^1.0.0" }
  ],
  "source": {
    "name": "source.tar.gz",
    "size": 12345,
    "digest": "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
    "tree": "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  },
  "artifacts": []
}
```

The descriptor contains the complete typed published intent, one source
archive reference, and declared artifacts. A consumer verifies the raw
descriptor against the lock before trusting any field.

The source archive contains one top-level `source/` directory and exactly one
embedded `source/vo.tree.json`. The tree index lists every packaged regular
file with normalized path, logical mode, size, and digest. It is the only file
closure manifest; there is no separately published package-manifest asset.

The descriptor's source digest authenticates the transport archive. `tree`
authenticates the exact embedded tree-index bytes. Tree entries authenticate
the extracted files. The packaged `vo.mod` intent must equal the descriptor
intent.

Archive consumers accept safe equivalent tar/gzip representations. They reject
links, devices, path traversal, duplicate paths, portable collisions, unknown
entry types, size bombs, and file-set drift. Publishers emit deterministic
archives for reproducible release output.

Artifacts are identified by kind, target, logical name, size, and digest.
Only artifacts required for the current target and reached extension modules
are materialized. Additional transport-provider assets do not invalidate a
release.

## 9. Verified cache

The v2 cache exposes one materialized directory per selected ModuleId/version:

```text
$VO_CACHE/v2/
  <encoded-module>/<version>/
  .vo-staging/
```

Each materialization contains the authenticated release descriptor, embedded
tree index, exact source closure, markers, and required artifacts. Installers
stream into private staging, validate the complete content, then publish with
atomic no-replace semantics. Concurrent requests for one selection converge on
the same validated directory. Digests remain the authority; the directory path
is only a lookup index.

Ordinary warm builds trust sealed user-owned cache objects. Source files are
checked while the compiler reads and fingerprints the files it actually uses.
`--deep` verification rehashes complete source trees and artifacts. A corrupt
object is quarantined and may be fetched and verified once more by exact
digest; recovery never requires clearing the complete cache.

Active read leases prevent cleaning while builds consume the cache. Browser
stores implement the same verified materialization contract in their VFS.

The fast-path threat model treats network and archive bytes as hostile, local
workspace source as developer-controlled mutable input, and the same user's
sealed cache as trusted. Stronger same-user tamper detection uses deep verify
and process sandboxing.

## 10. Planning and builds

The unified lifecycle is:

```text
ProjectRequest
  -> ProjectSnapshot
  -> hydrate exact descriptors
  -> ProjectPlan
  -> MaterializationPlan(target, purpose)
  -> MaterializedProject
  -> PreparedProject
  -> compile / run / check / test
```

`MaterializationPlan` is target- and purpose-aware. It lists each required
object once by digest, size, media type, role, and requiring module. Native and
browser consumers MUST produce the same object set from the same project plan,
target, and purpose.

Builds may fill the content-addressed cache with exact locked objects. They
never list versions or update the graph. A warm locked build performs zero
network requests.

The compiler validates import ownership while loading the actual package graph.
Project capture does not pre-scan every source file in every workspace member.
Build fingerprints include the plan ID and only the source files actually
consumed.

## 11. Studio sessions

Each Studio session owns an immutable plan, mounted trees, object leases,
runtime capabilities, a cancellation token, and a generation number. Runtime
capability identity includes `(session, generation, module, role)`. Closing a
session cancels pending work, unregisters capabilities, unmounts trees, and
releases cache leases.

Studio copies protocol metadata first, asks `ProjectContext` to validate the
exact selected graph, and copies source only for workspace members authorized
by workspace-origin lock records. Browser cache entries receive the same full
release, tree, source, and artifact validation as native cache entries.

## 12. CLI lifecycle

Module commands:

| Command | Effect |
|---|---|
| `vo mod add <module>@<constraint>` | atomically update intent and portable selection |
| `vo mod remove <module>` | atomically remove direct intent and repair selection |
| `vo mod update [module]` | reselect all modules, optionally preferring one named update |
| `vo mod sync [path]` | preserve valid selections and generate a registry-only lock |
| `vo mod tidy` | align direct intent with actual imports and repair selection |
| `vo mod verify [path]` | read-only validation of intent, selection, and cached bytes |
| `vo mod graph [path] [--declared] [--json]` | render the effective or declared graph |
| `vo mod why <module> [--declared]` | render the shortest dependency chain |
| `vo mod fetch [path]` | materialize exact selected objects |

Workspace commands:

| Command | Effect |
|---|---|
| `vo work sync [path]` | generate one mixed workspace/registry lock |

`vo cache clean` removes the active protocol cache. Release lifecycle uses
`vo release verify [path]` and `vo release stage [path] --out-dir <dir>`.

Public `mod snapshot` is removed; `mod graph --json` is its single replacement.

## 13. Structured diagnostics

Every module error contains a stable code, phase, optional module/version/path,
retryability, nested causes, and recovery actions. CLI text and `--json` are
renderings of the same structure. Hosts may map recovery actions to buttons.

Required codes include:

```text
LOCK_MISSING
LOCK_STALE
LOCK_INVALID
WORKSPACE_REQUIRED
WORKSPACE_ROOT_NOT_MEMBER
WORKSPACE_INTENT_DRIFT
PORTABLE_WORKSPACE_NODE
VERSION_CONFLICT
TOOLCHAIN_TOO_OLD
REGISTRY_UNAVAILABLE
REGISTRY_AUTH
REGISTRY_RATE_LIMITED
OFFLINE_MISS
OBJECT_CORRUPT
DIGEST_MISMATCH
TARGET_UNSUPPORTED
CONCURRENT_MUTATION
SESSION_EXPIRED
```

Solver failures carry the minimal incoming constraint chain. Offline and
materialization failures list every missing object. Cross-crate adapters retain
structured causes and never flatten an error into an untyped string.

## 14. Release and verification boundaries

Release build accepts only artifact identities declared by the public
`[extension.*]` contract. Local `[build.*]` metadata or an explicit staging
input may provide the bytes; staging validates the complete identity, target,
size, digest, and source snapshot before publishing output.

Portable release verification disables workspace overlays, rejects workspace
and local nodes, verifies a clean source snapshot, and checks the complete
target artifact contract.

## 15. Conformance

The repository maintains one hermetic registry fixture containing a root app,
source library, transitive library, native extension, and WASM/JavaScript
extension. The same fixture must pass CLI, native engine, `vo-web`, Studio Web,
Studio Tauri, corruption detection, target selection, and release tests.

Adapters given the same project, target, and purpose must produce the same plan
ID and required object digest set. Warm builds use no network. A low-frequency
live registry canary may detect provider drift but is never the sole
correctness gate.

Single-file inline modules remain dependency-free and use no lock or workspace.
