# Volang Repository Layout

Status: canonical

This document defines repository and release layout for modules using the
protocol in [`module.md`](./module.md).

## 1. Single-module repository

The recommended layout is:

```text
repo/
├── vo.mod
├── vo.lock                 # present when registry versions are selected
├── README.md
├── LICENSE
├── cmd/
│   └── app/
│       └── main.vo
├── internal/
├── pkg/
├── assets/
└── tests/
```

- `vo.mod` is required at the module root.
- `vo.lock` v3 is committed when registry versions provide graph authority. It
  is omitted for an empty graph and may be omitted when `vo.work` supplies the
  complete reachable dependency graph from local members.
- Executables normally live under `cmd/<name>`.
- `internal/` holds module-private packages; reusable packages can live under
  `pkg/` or another stable package path.
- Every tracked regular file inside the module boundary enters the release
  source closure. Runtime paths declared by `[web]` or `[extension.web.js]`
  must resolve inside that closure.
- `.volang/`, module caches, staging directories, Cargo `target/`, and generated
  binaries are local state and must stay untracked.

## 2. Extension module

Local build inputs are free to follow their native tool's conventions:

```text
repo/
├── vo.mod
├── vo.lock
├── pkg/
│   └── renderer/
├── rust/
│   ├── Cargo.toml
│   └── src/lib.rs
└── web/
    └── pkg/
```

`vo.mod` keeps two distinct contracts:

- `[extension.*]` declares public runtime identity, supported targets, and
  logical artifact names.
- `[build.*]` points to local Cargo, prebuilt, WASM, and JavaScript inputs.

Build output paths never define public artifact identity. Native/WASM binaries
stay untracked; release staging copies verified outputs into immutable release
assets. The raw `vo.mod`, including `[build.*]`, remains authenticated source
identity.

## 3. Multi-module repository

A Git repository may contain independent modules:

```text
repo/
├── modules/
│   ├── core/
│   │   ├── vo.mod
│   │   └── vo.lock
│   └── graphics/
│       ├── vo.mod
│       └── vo.lock
└── vo.work                  # optional local development file
```

Each module has its own identity, dependency graph, release verification, and
source package. A repository-level `vo.work` can list their directories for
local development; it has no effect on publication.

Publication discovers nested module boundaries from the immutable commit tree.
A parent module excludes every subtree rooted at a nested tracked `vo.mod`;
module manifest basenames must use the exact portable spelling `vo.mod`.

Workspace members use canonical portable paths relative to `vo.work`. Use `.`
or slash-separated names with any `../` components confined to the beginning;
do not use absolute paths, `./name`, backslashes, trailing slashes, or internal
`..` components. Member spellings must also remain distinct under portable
case-folding.

Tag placement follows the module path relative to the repository root:

| Module path | Version | Git tag |
|---|---:|---|
| `github.com/acme/repo` | `1.2.3` | `v1.2.3` |
| `github.com/acme/repo/modules/core` | `1.2.3` | `modules/core/v1.2.3` |
| `github.com/acme/repo/modules/core/v2` | `2.1.0` | `modules/core/v2/v2.1.0` |

Version values in protocol files remain bare semantic versions.

## 4. Release layout

Every valid published version has one immutable GitHub Release containing:

```text
vo.release.json              # format 1
source.tar.gz                 # fixed source asset name
<zero or more opaque artifact assets>
```

The source archive contains:

```text
source/
├── vo.tree.json             # authenticated by vo.release.json
├── vo.mod
└── <all files listed by vo.tree.json>
```

There is no browser-only release index. Native and browser consumers both use
`vo.release.json` and the embedded `vo.tree.json`.

## 5. CI and publication

At minimum, CI should:

1. format and check every module;
2. run its native and relevant WASM tests;
3. run `vo mod verify` after the cache has been fetched;
4. build every target promised by `[extension.native]` and
   `[extension.wasm]`;
5. run `vo release verify` with `VOWORK=off`;
6. stage a release in a temporary output directory and verify the exact asset
   set and digests.

Before tagging:

- `vo.mod` and the required `vo.lock` match the release commit;
- every dependency edge is represented by an authenticated release descriptor;
- the staging producer derives `vo.tree.json` from the complete committed
  raw-byte file closure inside the module boundary;
- the source archive embeds those exact tree-index bytes;
- every declared public artifact has exactly one staged byte payload;
- the worktree and release commit remain unchanged throughout staging.

Repository README text may summarize supported targets. The public contract in
`vo.mod` remains authoritative for runtime declarations. Remote source bytes
and modes are authoritative only after the immutable release assets and their
authenticated package closure have been verified.
