# Modules and dependencies

A Volang project records authored dependency intent in `vo.mod` and one exact,
authenticated selection in `vo.lock`. Build commands consume the selected graph
without silently changing it.

## Module identity

`vo.mod` uses TOML and declares a lowercase, host-qualified ModuleId:

```toml
format = 1
module = "example.com/acme/service"
version = "0.1.0"
vo = "0.1.4"

[dependencies]
"github.com/acme/records" = "^1.2.0"
```

`local/<name>` identifies an unpublished project. Imports contain the ModuleId
and package path with no version:

```vo
import "github.com/acme/records/codec"
```

## Select and materialize

The normal lifecycle is:

```sh
vo mod add github.com/acme/records@^1.2.0
vo mod sync
vo mod fetch
vo mod verify
```

- `add`, `update`, `remove`, `tidy`, and `sync` may select and atomically write
  a graph.
- `fetch` downloads only the exact digests already selected by `vo.lock`.
- `verify`, `graph`, and `why` are read-only.
- compilation requires a coherent graph and never performs version solving.

Commit `vo.mod` and every non-empty generated `vo.lock` with the source that
uses them.

## Lock contents

Lock format 1 contains exact nodes and authenticated descriptors. Registry
nodes bind release descriptor bytes; workspace nodes bind local module intent.
Dependency edges, toolchain requirements, source recipes, and extension
artifacts derive from those authenticated sources.

Use `vo mod graph --json` when tooling needs the canonical machine-readable
effective graph. Use `vo mod why MODULE` to explain the shortest selection
path.

## Workspaces

A workspace catalog lists module directories:

```toml
format = 1
members = ["app", "records", "tools/generator"]
```

The active module must be a listed member. The nearest matching ancestor
workspace is selected, and nested workspaces do not merge. `vo work sync`
produces one mixed workspace/registry lock. `vo work materialize` builds locked
workspace source recipes into the cache when the graph requires them.

Workspace substitution remains explicit in the lock: a directory is used only
for a node whose origin is `workspace`, and its identity, version, and intent
must still match.

## Inline modules

One dependency-free source file may carry a restricted inline module block:

```vo
/*vo:mod
format = 1
module = "local/demo"
version = "0.1.0"
vo = "0.1.4"
*/

package main
```

Inline modules do not read user locks, workspaces, registries, or caches.
Promote the script to a directory project before adding dependencies or more
source files.

## Extensions and release

`[extension.*]` describes a public loading contract. Local producer settings
belong under `[build.*]` and are excluded from published intent. Before
publication, run:

```sh
vo release verify .
vo release stage . --out-dir release
```

Verification requires committed, clean release source and a complete lock when
registry dependencies exist. Staging emits the versioned release descriptor,
source archive with its embedded tree manifest, and declared artifacts.

## Reproducibility rules

- Never edit `vo.lock` by hand.
- Run selection commands deliberately and review the graph change.
- Authenticate selected bytes before offline or release builds.
- Keep project intent separate from machine-local cache paths.
- Treat a dependency, toolchain, public metadata, or extension change as a new
  selection and release input.
