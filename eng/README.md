# Engineering System

`eng/` is the source-of-truth directory for Volang development automation.
Files here describe what to run, what tools are required, how CI plans work,
which generated artifacts are checked in, and how sibling first-party repos are
located.

Do not add new task graphs, tool version pins, release matrices, or language
test target definitions under `.github/`, `scripts/ci/`, `d.py`, or ad hoc
shell snippets. Add the data here and route execution through `vo-dev`.

## Files

- `tasks.toml`: named task graph, groups, dependencies, inputs, outputs, tool
  declarations, output publication policy, Node workspace usage, and
  first-party repo ownership.
- `toolchains.toml`: required tool versions, check commands, Rust cache
  workspaces, and Node/npm workspace lockfile policy.
- `tests.toml`: language test targets, aliases, default selections, target
  commands, target environments, and test policy constants.
- `ci.toml`: changed-file routing and named GitHub execution matrices.
- `project.toml`: Volang repo identity, first-party sibling repos, external
  project hints, CI checkout policy, and first-party workspaces.
- `artifacts.toml`: checked-in generated artifact registry, generator commands,
  validator commands, provenance files, and size/extension policy.
- `release.toml`: release package policy, target matrix, cross version,
  release-note text, and Homebrew metadata.

## Boundaries

- `vo-dev` is the only implementation that should interpret these files.
- `d.py` is a compatibility facade and should not duplicate selectors, target
  maps, tool versions, or task behavior.
- GitHub workflow YAML should request matrix and metadata from `vo-dev` instead
  of hard-coding task-specific tool setup.
- PR planning stays task-granular. The generated GitHub matrix compacts selected
  tasks into disjoint `ci-pr-*` groups, then resolves each dependency once inside
  its unit.
- Vehicle/physics integration, industrial soak, and replay workloads live in
  `site`/qualification; PR units keep source, focused unit, and browser-startup
  contracts.
- The `qualification` matrix runs weekly at UTC 03:37 and remains manually
  triggerable for releases. Its VM, Voplay, and release units are disjoint.
- Rust cache workspaces may belong to a declared first-party repo; matrix
  metadata includes those cache paths only when the unit checks out that repo.
- First-party repo layout is declared in `project.toml`; task commands should
  reference declared workspaces instead of embedding subdirectory paths.
- Checked-in generated artifacts must be listed in `artifacts.toml` and have a
  provenance file when the artifact policy requires one.

## Release protocol

`eng/release.toml` owns both the GitHub binary matrix and the ordered public
Rust SDK package set. A binary release tag is exactly `v` plus
`workspace.package.version`. `vo-dev release metadata` resolves that tag to the
checked-out commit, derives the RFC 3339 build date and `SOURCE_DATE_EPOCH` from
the commit, and rejects a changed checkout or conflicting workflow inputs.

Each target build writes a local build receipt that binds the target binary to
the tag, commit, version, timestamp, size, and SHA-256 digest. Packaging creates
a deterministic single-entry `tar.gz`, verifies its headers and contents, and
emits a canonical provenance JSON plus an exact checksum file. The release
publisher accepts only the complete artifact set declared by `release.toml`,
uploads it to a draft, verifies remote names, sizes, and server-reported SHA-256
digests, then publishes the draft and verifies the published state again.
Published releases are immutable through this command.

The crates.io SDK flow remains an explicit maintainer operation:

```sh
node scripts/ci/sdk_package_offline_consumer.mjs
cargo run -q -p vo-dev --offline --locked -- release sdk-plan --check
cargo run -q -p vo-dev --offline --locked -- release sdk-plan
```

The first two commands are network-offline preflight gates and do not publish.
The last command prints the topologically ordered `cargo publish --locked`
commands and does not run them. A maintainer with a crates.io token runs one
printed command at a time and waits for each package version to become available
before continuing. The repository has no token-consuming automatic SDK publish
job.
