# Module System

Every project owns authored intent in `vo.mod`. A project with dependencies
also owns one exact graph in `vo.lock`.

```toml
format = 1
module = "github.com/your-name/myapp"
version = "0.1.0"
vo = "0.1.0"

[dependencies]
"github.com/vo-lang/vopack" = "^0.1.2"
```

Public ModuleIds are lowercase host-qualified paths. `local/<name>` identifies
an unpublished project that can participate in a workspace or bundle. Imports
contain no version:

```vo
import "github.com/vo-lang/vopack"
```

## Dependency lifecycle

```bash
vo mod add github.com/vo-lang/vopack@^0.1.2
vo mod sync
vo mod fetch
vo mod verify
```

- `add`, `update`, `remove`, `tidy`, and `sync` select a graph atomically.
- `fetch` materializes only the exact digests selected by `vo.lock`.
- `verify`, `graph`, and `why` are read-only.
- Builds consume the frozen graph and never solve versions.

`vo.lock` format 1 contains only exact nodes. Registry nodes bind raw
`vo.release.json` bytes with `release`; workspace nodes bind local `vo.mod`
intent with `intent`. Dependency edges and toolchain requirements come from
those authenticated descriptors, so the lock has one source for each fact.

## Workspaces

A workspace is a directory catalog:

```toml
format = 1
members = ["app", "vopack", "vogui"]
```

Members are `.` or normalized descendants of the workfile directory. The
active project root must be listed. The nearest ancestor workfile is selected
only when it lists that root; nested workspaces never merge.

Run `vo work sync` to produce a mixed workspace/registry lock. A build uses a
workspace directory only for a node whose lock origin is `workspace`, and its
ModuleId, version, and intent must still match. Changing dependencies,
toolchain minimum, version, or public metadata requires another sync.

## Extensions and publication

`[extension.*]` declares the public loading contract. `[build.*]` contains
local producer inputs and is excluded from published intent. Release version
comes from `vo.mod`:

```bash
vo release verify
vo release stage --out-dir <dir>
```

The release consists of `vo.release.json`, `source.tar.gz`, and declared
artifacts. The archive embeds one `source/vo.tree.json`; there is no separate
tree asset.

## Inline modules

One dependency-free source file may embed the restricted schema:

```vo
/*vo:mod
format = 1
module = "local/demo"
version = "0.1.0"
vo = "0.1.0"
*/

package main
```

Inline modules do not read locks, workspaces, registries, or caches. Promote
the file to a normal project when it needs dependencies or multiple files.
