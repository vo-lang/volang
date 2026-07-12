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
  declarations, Node workspace usage, and first-party repo ownership.
- `toolchains.toml`: required tool versions, check commands, Rust cache
  workspaces, and Node/npm workspace lockfile policy.
- `tests.toml`: language test targets, aliases, default selections, target
  commands, target environments, and test policy constants.
- `ci.toml`: changed-file routing and the small set of GitHub execution lanes.
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
  its lane. Final signoff selectors remain standalone matrix jobs.
- Long industrial soak and replay workloads live in `site`/production readiness;
  PR lanes keep the corresponding source, unit, Scene3D, and browser contracts.
- First-party repo layout is declared in `project.toml`; task commands should
  reference declared workspaces instead of embedding subdirectory paths.
- Checked-in generated artifacts must be listed in `artifacts.toml` and have a
  provenance file when the artifact policy requires one.
