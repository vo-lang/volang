# Module System

Volang projects keep authored dependency intent and the selected graph in two
small files:

```text
myapp/
├── vo.mod
├── vo.lock        # required for registry-backed dependencies
└── main.vo
```

## `vo.mod`

`vo.mod` is TOML:

```toml
module = "github.com/your-name/myapp"
vo = "^0.1.0"

[dependencies]
"github.com/vo-lang/vopack" = "^0.1.2"
```

Choose the canonical `github.com/<owner>/<repository>[/subdirectory]` identity
when the project is created, including for local work that has not been pushed
yet. The reserved `local/<name>` namespace belongs only to inline modules and
toolchain-owned ephemeral state.

Imports stay versionless:

```vo
import "github.com/vo-lang/vopack"
```

Protocol versions are bare semantic versions (`1.2.3`, `^1.2.3`, or
`~1.2.3`). Git release tags keep the `v` prefix.

## Dependency lifecycle

Use separate commands for selection and cache materialization:

```bash
vo mod add github.com/vo-lang/vopack@^0.1.2
vo mod sync
vo mod fetch
vo mod verify
```

- `add`, `update`, `remove`, `tidy`, and `sync` solve the graph and write
  `vo.mod`/`vo.lock`; they do not download packages.
- `fetch` authenticates the versions already pinned by `vo.lock` and fills the
  module cache.
- `verify`, `why`, `graph`, and `snapshot` are read-only.
- `vo cache clean` explicitly removes every installed version from the active
  cache generation while retaining its owned root and protocol infrastructure.

Native tools use the versioned default cache root `~/.vo/mod/v1`.
`VO_MOD_CACHE` selects an exact, non-empty absolute alternative root. Entries
in older unowned cache layouts remain untouched and are never adopted
implicitly. If the tool cannot determine an absolute user home directory,
cache-root selection fails until an absolute `VO_MOD_CACHE` is supplied.

`vo.lock` v3 stores selected module versions, toolchain constraints, release
digests, and dependency edges. Release/package manifests supply source files
and artifacts after their bytes have been authenticated.

When `[web].entry` is present, it names a normalized portable `.vo` path
relative to the module root, such as `cmd/web/main.vo`.

## Inspect the graph

```bash
vo mod why github.com/vo-lang/vopack
vo mod graph
vo mod snapshot
```

`snapshot` exports canonical JSON for Studio and other first-party tooling.
`graph`, `snapshot`, and `why` show the active workspace authority by default.
Add `--declared` to inspect the registry graph with workspaces disabled.

## Workspaces

`vo.work` v1 lists member directories. Each member's identity comes from its
own `vo.mod`:

```toml
version = 1
members = [".", "../vopack", "../vogui"]
```

The nearest ancestor `vo.work` is active by default. Set `VOWORK=off` for the
declared registry graph. With a lock, workspace members select local source for
matching locked modules while versions and published identities stay unchanged.
Without a lock, the selected workspace is valid only when its members cover the
complete reachable dependency closure. Any missing, open, or registry-backed
edge requires the complete v3 lock. Version constraints remain the project's
publication intent. Declared graph views, `VOWORK=off`, dependency verification,
and release verification always require the locked registry graph.

Member paths use `/` and are relative to `vo.work`. `.` and leading `../`
components are valid. Absolute paths, `./name`, backslashes, trailing slashes,
internal `..` components, and spellings that collide under portable
case-folding are rejected.

## Extensions

Public runtime metadata and local build inputs use separate sections:

```toml
[extension]
name = "myext"

[extension.native]
library = "vo_myext"
targets = ["aarch64-apple-darwin"]

[build.native]
kind = "cargo"
manifest = "rust/Cargo.toml"
package = "myext"
```

`[extension.*]` declares what consumers can load. `[build.*]` tells local
tooling how to produce those bytes and never becomes published identity.
The optional native `library` is a portable public stem and may differ from
the Cargo package or library target; platform filenames are derived from it.

Browser modules use the same authenticated `vo.release.json` v2 and
`vo.package.json` v1 as native modules. The package manifest lists the exact
raw-byte file closure.

## Publication

Run `vo release verify` on a clean commit before staging. Verification checks
the committed module graph, publish closure, and local build inputs without
writing release output. `vo release stage --version 1.2.3 --out-dir <dir>`
then creates the exact release/package manifests, source archive, and declared
artifacts. Protocol versions stay bare; the Git tag is `v1.2.3`.

## Inline modules

A single source file can embed a restricted TOML manifest:

```vo
/*vo:mod
module = "local/demo"
vo = "^0.1.0"
*/

package main
```

Inline modules are isolated from workspaces and support only `module` and
`vo`. Promote the file to a project when it needs external dependencies,
publication, multiple files, or extension metadata.
