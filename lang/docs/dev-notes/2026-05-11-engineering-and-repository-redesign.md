---
date: 2026-05-11
status: implemented
area: repo-layout
owner: volang
supersedes: []
superseded_by: []
---

# Volang Engineering and Repository Redesign

**Date**: 2026-05-11
**Status**: Canonical design and implementation record.
**Scope**: Rebuild Volang's engineering control plane and repository source-of-truth layout in one design: tasks, tools, tests, CI, release, artifacts, first-party repos, docs, examples, command crates, apps, local state, fixtures, benchmarks, and historical notes.
**Cutover policy**: No transition period. Each implemented cutover step must leave its touched surface in the target layout, with no old-path state accepted as complete.

---

## 1. Purpose

Volang has working compiler/runtime/app code, but its development support system and repository layout have accumulated too many independent sources of truth.
This document defines the single target design.

The redesign has two inseparable parts:

1. **Engineering control plane**: `vo-dev`, `eng/*.toml`, CI planning, toolchain checks, test planning, artifact policy, release packaging, first-party repo boundaries, and thin developer entrypoints.
2. **Repository layout and source ownership**: physical homes for tests, docs, examples, app roots, command crates, local state, fixtures, benchmark metadata, and historical docs.

The design is intentionally strict:

- every task is declared once;
- every tool version is declared once;
- every release target and packaging rule is declared once;
- every test target is generated from manifest data;
- every committed generated artifact has an owner and validation rule;
- every first-party sibling dependency is represented as data;
- every major source surface has one authoring root;
- old source paths are deleted in the same cutover step that installs the replacement.

---

## 2. Final Repository Layout

The target physical layout is:

```text
volang/
├── apps/
│   ├── studio/
│   │   ├── docs/
│   │   │   ├── manifest.toml
│   │   │   └── pages/
│   │   ├── package.json
│   │   ├── public/
│   │   ├── src/
│   │   ├── src-tauri/
│   │   └── wasm/
│   └── playground-legacy/
│       ├── package.json
│       ├── public/
│       ├── rust/
│       └── src/
│           └── assets/docs/generated/
├── benchmarks/
│   ├── manifest.toml
│   ├── <benchmark-id>/
│   └── results/
├── cmd/
│   ├── vo/
│   │   ├── Cargo.toml
│   │   └── src/main.rs
│   ├── vo-dev/
│   │   ├── Cargo.toml
│   │   └── src/main.rs
│   ├── vo-embed/
│   │   ├── Cargo.toml
│   │   └── src/main.rs
│   └── vo-test/
│       ├── Cargo.toml
│       ├── README.md
│       └── src/main.rs
├── eng/
│   ├── README.md
│   ├── artifacts.toml
│   ├── ci.toml
│   ├── project.toml
│   ├── release.toml
│   ├── tasks.toml
│   ├── tests.toml
│   └── toolchains.toml
├── examples/
│   ├── manifest.toml
│   ├── basics/
│   ├── concurrency/
│   ├── gui/
│   ├── integration/
│   └── modules/
├── lang/
│   ├── crates/
│   ├── docs/
│   │   ├── archive/
│   │   ├── dev/
│   │   ├── dev-notes/
│   │   ├── guides/
│   │   └── spec/
│   └── stdlib/
├── scripts/
│   └── ci/
├── tests/
│   ├── fixtures/
│   └── lang/
│       ├── manifest.toml
│       ├── archives/
│       ├── cases/
│       ├── fixtures/
│       └── projects/
├── .volang/
│   ├── cache/
│   ├── apps/studio/
│   └── tmp/
├── Cargo.toml
├── Cargo.lock
├── README.md
├── agent.md
├── d.py
├── rust-toolchain.toml
└── vo.work
```

Root-level `studio`, `playground`, `.examples`, `lang/test_data`, `cmd/vo-test/rust`, `.vo-cache`, `.volang/studio/sessions`, `.volang/studio/sources`, root scratch `.vo` programs, and root `.vob` bytecode outputs are not part of the target layout.

---

## 3. Ownership Table

| Concern | Authoritative owner | Removed or generated paths |
| --- | --- | --- |
| task graph | `eng/tasks.toml` + `cmd/vo-dev` | workflow-local task commands |
| changed-file planning | `eng/ci.toml` + `cmd/vo-dev` | Python CI planners |
| tool versions | `eng/toolchains.toml`, `rust-toolchain.toml` | ad hoc workflow pins |
| language test targets | `eng/tests.toml` | runner-local target maps |
| language test cases | `tests/lang/manifest.toml` + `tests/lang` | `lang/test_data` |
| native test execution | `cmd/vo-test run-plan` | `vo-test <mode>` discovery |
| artifact policy | `eng/artifacts.toml` + `vo-dev lint artifacts` | undeclared checked-in generated payloads |
| first-party repos | `eng/project.toml` + `vo-dev first-party ...` | raw `../repo` task paths |
| release packaging | `eng/release.toml` + `vo-dev release ...` | workflow-local release matrices |
| language specs | `lang/docs/spec` | manually edited app spec mirrors |
| Studio help docs | `apps/studio/docs` | copied language specs |
| Playground docs | generated under `apps/playground-legacy/src/assets/docs/generated` | `apps/playground-legacy/src/assets/docs/generated` |
| examples | `examples/manifest.toml` + `examples` | `.examples` |
| command crates | `cmd/<name>` | `cmd/<name>/rust` for command crate roots |
| app roots | `apps/studio`, `apps/playground-legacy` | root `studio`, root `playground` |
| repo-local state | `.volang` | `.vo-cache`, `.volang/studio/sessions`, `.volang/studio/sources` |
| checked-in fixtures | `tests/fixtures` or owner-specific `fixtures` | generic root `assets` without manifest |
| benchmarks | `benchmarks/manifest.toml` | implicit directory discovery |
| historical docs | `lang/docs/dev-notes` with metadata, `lang/docs/archive` | status-free stale notes |

---

## 4. Exact Design Goals

### Goal G1: One Authoritative Task Graph

All build, check, test, package, smoke, release-verify, deploy-build, and local utility tasks must be declared in `eng/tasks.toml`.

Success criteria:

- `vo-dev task list` prints every runnable task.
- `vo-dev task run <task>` and GitHub Actions execute the same task definition.
- GitHub workflows do not contain hand-written commands for tasks that exist in `eng/tasks.toml`.
- `./d.py ci <selector>` delegates to `vo-dev task run <selector>`.
- Python CI planning/execution scripts are deleted; remaining `scripts/ci` files are narrow helpers.

### Goal G2: Local And CI Task Planning Are Identical

The changed-file planner must run from the same implementation locally and in CI.

Success criteria:

- local CI and GitHub CI both reach the same `cmd/vo-dev` planning implementation.
- local execution uses `vo-dev task run`; GitHub Actions uses `vo-dev ci matrix`.
- single-job workflows use `vo-dev ci metadata <selector>` for tool versions, Node lockfiles, and first-party checkout metadata.
- unknown changed paths trigger the configured conservative fallback set.
- untracked files are included in local planning.
- GitHub planning uses explicit base/head SHAs; invalid explicit SHAs are hard failures.
- selected tasks pull in downstream tasks listed through `needs`.

### Goal G3: Toolchain Versions Are Data

Rust, Node, npm, wasm-pack, Python requirements, and auxiliary tools must be declared in `eng/toolchains.toml`.

Success criteria:

- local and CI Node versions come from `eng/toolchains.toml`.
- local and CI wasm-pack versions come from `eng/toolchains.toml`.
- tools supplied by another toolchain use `version_from`.
- Rust still uses `rust-toolchain.toml`, and `eng/toolchains.toml` references it.
- `vo-dev tool check` fails with actionable messages for missing or wrong-version tools.
- `vo-dev` does not depend on Python TOML support.

### Goal G4: Release Packaging Is Data

Release targets, runner OSes, cross version, package naming, release notes, and Homebrew formula mutation must be declared in `eng/release.toml` and executed by `vo-dev release`.

Success criteria:

- `.github/workflows/release.yml` gets its target matrix from `vo-dev release matrix`.
- `cross` installation uses `vo-dev release cross-version`.
- Homebrew tap checkout uses `vo-dev release homebrew-metadata`.
- release publication rejects missing or extra artifact files.
- release publication and Homebrew updates reject malformed checksums and checksum/tarball mismatches.
- Homebrew formula updates reject target sets that differ from `eng/release.toml`.
- `vo-dev lint release` validates release policy.
- workflow YAML only performs checkout, setup, artifact transfer, and the final Homebrew tap git operation.

### Goal G5: Test Matrix Is Explicit And Complete

Language tests must be represented as test cases multiplied by declared targets.
The matrix includes native VM, native JIT, GC VM, GC JIT, no_std/embed, WASM, OSR, and compile diagnostics.

Success criteria:

- `vo-dev test plan --suite lang --format json` emits the exact jobs that will run.
- every skip has a target and reason.
- normal file/pass tests default to required targets from `eng/tests.toml`.
- omitting a required target is a lint error unless the case has a reason or is a GC-specific regression.
- GC-named regressions require all targets from the GC regression alias.
- non-blank test cases may not skip every declared target.
- every expected failure has a diagnostic pattern or named error code.
- expected failures are planned for native compile diagnostics and WASM compile diagnostics when selected.
- blank `.vo` files are hard errors unless explicitly marked as placeholders.
- missing manifest entries, duplicate entries, and missing files are lint errors.
- `vo-test` executes a plan; it does not own discovery or target expansion.

### Goal G6: Runtime Mode Behavior Is Testable

Runtime-specific settings such as `VO_JIT_CALL_THRESHOLD`, `VO_JIT_LOOP_THRESHOLD`, and `VO_GC_DEBUG` must be part of generated test plans.

Success criteria:

- GC jobs contain `VO_GC_DEBUG=1`.
- OSR jobs contain `VO_JIT_CALL_THRESHOLD=1000` and `VO_JIT_LOOP_THRESHOLD=1`.
- runners do not derive runtime behavior from filenames.
- each test job records binary, target, environment, timeout, and expected result in machine-readable output.

### Goal G7: Artifacts Are Classified Before Commit

Generated outputs, release artifacts, cached outputs, checked-in fixtures, and deployable static packages must have explicit policy.

Success criteria:

- `vo-dev lint artifacts` scans tracked files and fails on undeclared large artifacts, generated WASM/JS glue, `dist`, `pkg`, and large JSON files.
- checked-in quickplay content is represented in `eng/artifacts.toml` with owner, generator, validator, size cap, and provenance.
- quickplay validators, smoke tests, and Studio build-id hashing derive artifact paths from checked-in package metadata.
- ignored build directories remain untracked and are validated by policy.
- every tracked generated artifact has a reproducibility contract.
- root generic binary fixtures are moved to an owner fixture directory and declared.

### Goal G8: First-Party Repository Boundaries Are Explicit

The system must know which first-party repositories are optional, required, checked out by CI, or discovered locally.

Success criteria:

- `eng/project.toml` declares `vogui`, `voplay`, `vopack`, `vostore`, and `BlockKart`.
- tasks do not reach into sibling repos or module caches without declared policy.
- CI checkout repository names and checkout paths are emitted by `vo-dev`.
- local sibling paths are hints only unless a task explicitly requires a local repo.

### Goal G9: WASM Build Behavior Has One Owner

All WASM builds must be expressed as task graph nodes with declared inputs and outputs.

Success criteria:

- `vo-web` WASM, Studio WASM, Playground legacy WASM, and WASM tests are all task graph nodes.
- staleness is computed by `vo-dev` from task inputs and outputs.
- CI does not run `wasm-pack` directly outside the task runner.
- WASM language-test build and runner commands live on the `wasm` target in `eng/tests.toml`.
- `vo-dev test run --targets wasm` does not hard-code `lang/crates/vo-web` outside config.
- WASM execution consumes the same `volang.test-plan.v1` jobs as native execution.
- WASM diagnostic pattern matching uses the same ordered `X` wildcard segmentation as native diagnostics.

### Goal G10: Developer Entrypoints Are Thin

Developer-facing wrappers may exist only as thin entrypoints over `vo-dev`.

Success criteria:

- `./d.py test both` delegates to `vo-dev test run --suite lang --targets vm,jit`.
- `./d.py ci smart` delegates to `vo-dev task run smart`.
- `./d.py studio` delegates task decisions to `vo-dev`.
- test aliases live in `eng/tests.toml`; `d.py` does not maintain a target map.
- `d.py` consumes wrapper-only flags only for commands that document them.
- forwarded `vo` and default commands preserve arguments.
- every wrapper command has a `vo-dev` owner.

### Goal G11: Every Logical Surface Has One Authoring Root

For every major source surface, the repository must identify one authoring root.
Generated copies must be declared as generated.

Success criteria:

- each surface in the ownership table has one owner and one authoring root;
- old source paths are deleted in the same cutover step that installs the replacement;
- `vo-dev lint layout` reports split-brain ownership before CI accepts a change.

### Goal G12: Test Source Files Live Under `tests/lang`

The manifest and test corpus must share the same owner.

Rules:

- `tests/lang/manifest.toml` uses `root = "tests/lang"`.
- file cases live under `tests/lang/cases`.
- project cases live under `tests/lang/projects`.
- zip/archive cases live under `tests/lang/archives`.
- shared test data lives under `tests/lang/fixtures`.
- no new or existing executable test case remains directly under the `tests/lang` root after the test-corpus cutover; executable files live under `tests/lang/cases`.
- `lang/test_data` is deleted after the manifest no longer references it.

Success criteria:

- `rg -n "lang/test_data" tests/lang eng cmd/vo-dev d.py skills/volang-dev` returns no executable or workflow references except explicit layout-lint rejection messages.
- `vo-dev test lint --suite lang` passes with `root = "tests/lang"`.
- `vo-dev test plan --suite lang --targets vm,jit --format json` emits paths under `tests/lang`.
- `lang/test_data` is absent from `git ls-files`.

### Goal G13: Documentation Has One Spec Source

`lang/docs/spec` is the authoring root for language, module, FFI, bytecode, memory, and JIT specs.
Playground and Studio may present docs, but they must not maintain independent copies of spec content.

Rules:

- spec docs are authored only under `lang/docs/spec`.
- manually maintained docs under `apps/playground-legacy/src/assets/docs/spec` are removed.
- Playground-visible docs are generated under `apps/playground-legacy/src/assets/docs/generated`.
- generated Playground docs include source path, source digest, generator command, and generation timestamp.
- Studio docs under `apps/studio/docs` may contain app help and tutorials, but must link to `lang/docs/spec` for specification content.
- if a Studio page adapts spec content, it lists the source spec file in `apps/studio/docs/manifest.toml`.
- docs sync is a declared task, not a manual copy.

Success criteria:

- `language.md`, `module.md`, and `native-ffi.md` cannot diverge between `lang/docs/spec` and app-visible generated copies.
- no manually edited spec file remains under `apps/playground-legacy/src/assets/docs/spec` or `apps/playground-legacy/src/assets/docs/generated`.
- docs tasks are declared in `eng/tasks.toml`.
- CI quality includes docs lint.

### Goal G14: Examples Are Visible And Indexed

Examples are product-facing source files and must not live in ignored hidden directories.

Rules:

- `.examples` is removed from `git ls-files`.
- `.gitignore` no longer ignores `/examples/`.
- every runnable example is listed in `examples/manifest.toml`.
- example metadata includes `id`, `path`, `kind`, `description`, `expected_targets`, and `owner`.
- GUI examples declare whether they require Studio, browser runtime, native runtime, or first-party dependencies.
- examples used by tests are referenced by id or copied into tests, not imported from hidden paths.

Success criteria:

- `git ls-files .examples` prints nothing.
- `vo-dev lint examples` rejects unlisted `.vo` files under `examples`.
- `vo-dev task run task:examples-smoke` runs smokeable examples.

### Goal G15: Command Crate Roots Are Standard

Every command under `cmd` must have one conventional crate or source root.
`cmd/vo-test/rust` must not remain the long-term layout.

Rules:

- the Cargo workspace member is `cmd/vo-test`, not `cmd/vo-test/rust`.
- old Vo runner files are deleted from `cmd/vo-test`.
- archived runner history, if retained, moves to docs archive or tests fixtures, not `cmd/vo-test`.
- `cmd/vo-test` remains an execution engine; discovery and target expansion stay in `vo-dev`.

Success criteria:

- root `Cargo.toml` has `cmd/vo-test` as the member and has no `cmd/vo-test/rust` member.
- `rg -n "cmd/vo-test/rust" Cargo.toml cmd eng d.py .github skills` returns no current-code references except explicit layout-lint rejection messages.
- `cargo check -p vo-test` works.

### Goal G16: App Roots Are Explicit

Studio is the primary app.
Playground is legacy unless a later product decision re-promotes it.

Rules:

- Studio lives at `apps/studio`.
- Playground lives at `apps/playground-legacy`.
- task names and docs call Playground `playground-legacy`.
- `vo-web` remains a Rust crate under `lang/crates/vo-web`.
- shared browser/runtime code moves to crates or shared app libraries, not duplicated between app roots.
- app-specific docs and assets stay under the owning app root.

Success criteria:

- app tasks run from `apps/*`.
- no top-level `studio` or `playground` app roots remain.
- no new code imports from root `playground`.
- `vo-dev lint layout` rejects undeclared top-level app directories.

### Goal G17: Volang-Local State Is Under `.volang`

The repo has one hidden local-state root for Volang-specific developer state.

Rules:

- repo development commands use `.volang/cache/vo` instead of repo-root `.vo-cache`.
- Studio local session state uses `.volang/apps/studio/sessions`.
- Studio cloned/opened sources use `.volang/apps/studio/sources`.
- `.volang/studio/sessions` and `.volang/studio/sources` are removed.
- root `main.vob` and other bytecode outputs are not durable repo files.
- `.gitignore` ignores `.volang/`.
- `eng/artifacts.toml` declares local-state policy, or layout lint has the same allowlist.

Success criteria:

- normal repo commands do not create `.vo-cache`, `.volang/studio/sessions`, or `.volang/studio/sources` at repo root.
- `./d.py clean all` removes repo-local Volang state without touching user home caches.
- artifact/layout lint rejects new hidden top-level state roots.

### Goal G18: Root Files Are Controlled By Allowlist

The repository root contains project entrypoints and configuration, not scratch programs or build outputs.

Allowed root file classes:

- repository metadata: `README.md`, `LICENSE`, `agent.md`;
- Rust/workspace config: `Cargo.toml`, `Cargo.lock`, `rust-toolchain.toml`, `.cargo/config.toml`;
- Volang workspace config: `vo.work`;
- developer entry point: `d.py`;
- git/config files: `.gitignore`, `.github/**`.

Rules:

- `hello.vo` moves to `examples/basics/hello.vo` or is deleted if duplicated.
- `main.vob` is a build output and must not exist in `git ls-files`.
- generic root `assets` is removed unless it contains hand-authored shared assets with an owner manifest.
- `apps/studio/fixtures/blockkart/blockkart.vpak`, if kept as a fixture, moves to `apps/studio/fixtures/blockkart/blockkart.vpak` or `tests/fixtures/blockkart/blockkart.vpak` and is declared in `eng/artifacts.toml`.
- large binary fixtures have owner, purpose, max size, and validation policy.

Success criteria:

- `vo-dev lint layout` fails on unapproved root files.
- no tracked bytecode output exists at the repo root.
- every tracked binary fixture has an explicit artifact or fixture declaration.

### Goal G19: Benchmarks Are Manifest-Driven

Benchmarks must not be discovered only from directory names.

Rules:

- `benchmarks/manifest.toml` declares benchmark id, path, category, default status, languages, required tools, and outputs.
- `vo-dev bench` reads `benchmarks/manifest.toml`.
- generated results stay under `benchmarks/results`.
- benchmark build products are ignored and covered by clean policy.
- benchmarks that need external tools declare them in manifest and toolchain policy.

Success criteria:

- adding a benchmark directory without a manifest entry fails lint.
- `vo-dev bench all` and CI benchmark tasks select from the manifest.
- benchmark outputs are covered by artifact or generated-output policy.

### Goal G20: Historical Docs Have Lifecycle Metadata

Design notes remain useful without pretending to be current architecture.

Required front matter for new or touched design notes:

```markdown
---
date: 2026-05-11
status: design | implemented | superseded | archived
area: repo-layout
owner: volang
supersedes: []
superseded_by: []
---
```

Rules:

- new dev notes include status metadata.
- docs marked `superseded` link to replacements.
- docs under `lang/docs/outdated` move to `lang/docs/archive` or receive metadata explaining why they remain separate.
- skill/reference docs may cite design notes only with current/proposed/historical status clear from context.

Success criteria:

- `vo-dev lint docs` rejects new dev notes without status metadata.
- stale notes are searchable by status.
- onboarding docs point to current references before historical notes.

---

## 5. `vo-dev` Command Contract

`vo-dev` is the engineering control-plane binary under `cmd/vo-dev`.
It owns engineering behavior; `d.py` is a thin facade.

Required subcommands:

```text
vo-dev task list
vo-dev task show <task>
vo-dev task plan <selector> [--changed] [--base <sha>] [--head <sha>] [--format text|json]
vo-dev task run <selector> [--changed] [--base <sha>] [--head <sha>]

vo-dev ci matrix <selector> [--base <sha>] [--head <sha>] [--github-output]
vo-dev ci metadata <selector> [--github-output]

vo-dev tool check [--task <task>] [--json]
vo-dev tool bootstrap [--task <task>] [--apply]
vo-dev tool version <tool>

vo-dev verify plan <selector> [--changed] [--base <sha>] [--head <sha>]
vo-dev verify run <selector> [--changed] [--base <sha>] [--head <sha>]

vo-dev test lint --suite <suite>
vo-dev test plan --suite <suite> [--targets <list>] [--path <file-or-dir>] [--format json]
vo-dev test run --suite <suite> [--targets <list>] [--path <file-or-dir>] [-j <n>] [--format text|json] [--verbose] [--release]

vo-dev lint all
vo-dev lint tasks
vo-dev lint artifacts
vo-dev lint repo-boundaries
vo-dev lint release
vo-dev lint layout
vo-dev lint docs
vo-dev lint examples
vo-dev lint benchmarks

vo-dev first-party path <repo> [subdir]
vo-dev first-party ci-checkout <repo> [--github-output]
vo-dev first-party run <repo> <subdir> -- <command...>
vo-dev first-party run-workspace <repo> <workspace> -- <command...>
vo-dev first-party release-verify <repo>

vo-dev release matrix [--github-output]
vo-dev release cross-version
vo-dev release homebrew-metadata [--github-output]
vo-dev release build <target>
vo-dev release notes
vo-dev release publish
vo-dev release update-homebrew

vo-dev bench [all|vo|score|<name>] [--all-langs]
vo-dev loc [--with-tests]
vo-dev clean [all|vo|rust|bench|junk]
vo-dev studio [--build-wasm] [--build-only] [--runner] [project]
vo-dev studio-native [--build-wasm] [--runner] [project]
vo-dev studio-stop
```

Exit code classes:

- `0`: success.
- `1`: task/test/lint failure.
- `2`: invalid user input or config schema.
- `3`: missing required tool.
- `4`: missing required external repository or artifact.
- `124`: timeout.

---

## 6. Engineering Data Files

### `eng/tasks.toml`

Owns named task graph, groups, dependencies, inputs, outputs, tools, Node workspace usage, first-party repo ownership, and task timeouts.

Task lint checks:

- task names and group names are unique ASCII slugs;
- dependencies refer to known tasks;
- no dependency cycles exist;
- `tools` entries exist in `eng/toolchains.toml`;
- `node` tasks declare `node_workspaces`;
- first-party Node commands use declared workspaces;
- task inputs/outputs are repo-relative and concrete;
- generator outputs are covered by artifact policy;
- `vo-dev test run --targets wasm` tasks include tools inferred from the WASM target commands in `eng/tests.toml`.

### `eng/ci.toml`

Owns changed-file planning policy and fallback routing.

Rules:

- unknown changed paths run conservative fallback tasks;
- changed-file matching selects downstream tasks listed through `needs`;
- local planning includes untracked files;
- GitHub planning uses explicit base/head SHAs.

### `eng/toolchains.toml`

Owns required tool versions, check commands, install hints, Rust cache workspaces, and Node/npm workspace lockfile policy.

Rules:

- `npm` uses the Node version source when appropriate.
- Rust cache workspace lines are emitted by `vo-dev ci matrix` and `vo-dev ci metadata`.
- Node cache lockfiles are computed from selected task closure.
- first-party Node workspaces declared here must match `eng/project.toml`.

### `eng/tests.toml`

Owns language test targets, aliases, default selections, target commands, target environments, and test policy constants.

Required target kinds:

- native VM;
- native JIT;
- GC VM;
- GC JIT;
- OSR;
- no_std/embed;
- WASM;
- compile diagnostics.

Rules:

- aliases such as `both`, `gc`, and `embed` live here.
- target commands are parsed by task lint for tool inference.
- WASM build and runner commands live on the WASM target.
- coverage policy constants live here, not in Rust constants.

### `tests/lang/manifest.toml`

Owns language test cases.

Required fields:

- `id`: stable unique test id.
- `kind`: `file`, `project`, or `zip`.
- `path`: path relative to `tests/lang`.
- `expect`: `"pass"` or diagnostic failure data.

Optional fields:

- `targets`;
- `skip`;
- `reason`;
- `zip_root`;
- `timeout`: target-specific timeout overrides, for example `{ jit = 180 }`;
- `blank`.

Rules:

- every manifest path exists;
- every executable test file/project/archive is listed;
- skipped cases include reasons;
- timeout overrides reference declared, non-skipped targets and are greater than zero;
- expected failures include diagnostic patterns;
- non-failing cases have at least one target;
- blank placeholders are explicitly marked.

### `eng/artifacts.toml`

Owns generated/committed/deploy artifact policy.

Artifact classes:

- `source`;
- `fixture`;
- `generated-checked-in`;
- `build-output`;
- `release-output`;
- `cache`.

Rules:

- tracked `.wasm` files need artifact entries.
- tracked JSON files above the policy threshold need artifact entries.
- tracked `dist`, `pkg`, or `target` directories fail lint unless explicitly approved.
- ignored generated outputs are declared as build output or cache if tasks create them.
- generator/validator commands use `["vo-dev", "task", "run", "task:<task>"]`.
- generated checked-in artifacts carry provenance when policy requires it.
- structured artifact inputs reference declared repos.

### `eng/project.toml`

Owns Volang repo identity, first-party sibling repos, external project hints, CI checkout policy, and first-party workspaces.

Rules:

- tasks can reference `repo = "<name>"` only if the repo is declared.
- task-to-repo dependency lives on tasks, not duplicated in project data.
- first-party workspace subdirectories used by tools are declared under the owning repo.
- CI checkout behavior is generated from this file.
- local path discovery checks declared hints only.
- tasks that require local projects declare that requirement.
- tasks that intentionally use `vo.work` declare `workspace_mode = "allow"`.

### `eng/release.toml`

Owns release package policy, target matrix, cross version, release notes, checksums, and Homebrew metadata.

Rules:

- release target sets are explicit.
- build args must build the declared package.
- publication accepts only declared artifact/checksum sets.
- Homebrew target sets must match release targets.

### `examples/manifest.toml`

Owns example indexing.

Required fields:

- `id`;
- `path`;
- `kind`;
- `description`;
- `expected_targets`;
- `owner`.

GUI example metadata must declare runtime and dependency requirements.

### `benchmarks/manifest.toml`

Owns benchmark discovery and execution metadata.

Example:

```toml
[[benchmark]]
id = "call-dispatch"
path = "call-dispatch"
category = "runtime"
default = true
languages = ["vo", "go", "c"]
tools = ["vo", "hyperfine"]
outputs = ["benchmarks/results/call-dispatch.json"]
```

### Docs manifests

`apps/studio/docs/manifest.toml` owns Studio docs pages and any adapted spec sources.
Generated Playground docs must include a source manifest with source path, digest, generator command, and generation timestamp.

---

## 7. Test System Design

The test system has three layers:

1. `eng/tests.toml` defines target types, aliases, target commands, default selections, and policy constants.
2. `tests/lang/manifest.toml` defines test cases under `tests/lang`.
3. `vo-test` executes generated plan files.

`vo-test` must not:

- discover arbitrary files by walking the filesystem;
- infer GC behavior from filenames;
- infer OSR behavior from command mode;
- decide skip lists;
- decide expected-failure semantics.

`vo-test` must:

- read a test plan JSON file;
- execute each job;
- apply timeout and environment variables;
- cap default worker count at 4 unless `--jobs` explicitly overrides it;
- emit periodic text progress and a complete failure summary for long matrices;
- compare actual result with expected result;
- emit machine-readable results.

Generated plan schema root:

```json
{
  "schema": "volang.test-plan.v1",
  "suite": "lang",
  "jobs": [
    {
      "id": "append-self-slice::vm",
      "case_id": "append-self-slice",
      "kind": "file",
      "path": "tests/lang/cases/runtime/append_self_slice.vo",
      "target": "vm",
      "backend": "vm",
      "env": { "VOWORK": "off" },
      "timeout_sec": 20,
      "expect": { "kind": "pass" }
    }
  ]
}
```

Result schema root:

```json
{
  "schema": "volang.test-result.v1",
  "suite": "lang",
  "passed": 1,
  "failed": 0,
  "skipped": 0,
  "jobs": []
}
```

Text output may summarize JSON.
GitHub annotation conversion must be a separate adapter that consumes the structured result.

---

## 8. WASM, Studio, And App Build Design

WASM builds are task graph nodes.

Required task classes:

- `vo-web-wasm-build`;
- `studio-wasm-build`;
- `playground-legacy-wasm-build`;
- `vo-test-wasm`;
- `studio-build`;
- `site`.

Rules:

- `vo-dev studio` reads task definitions and computes staleness from declared inputs and outputs.
- local launch logic does not carry separate hard-coded WASM source lists.
- Studio build ID is produced by the Studio WASM task and validated by artifact policy.
- Studio quickplay inputs are part of the Studio build-id task inputs.
- deploy workflow uses `vo-dev task run site`.
- Playground tasks are named `playground-legacy-*`.
- app roots are `apps/studio` and `apps/playground-legacy`.
- `vo-web` stays under `lang/crates/vo-web`.

---

## 9. Required Lints

### `vo-dev lint tasks`

Checks:

- schema validity;
- task/group uniqueness;
- unknown dependency references;
- cycles;
- missing tools;
- missing Node workspace declarations;
- first-party command boundary violations;
- output coverage by artifact policy;
- test-target command tool inference.

### `vo-dev lint artifacts`

Checks:

- schema validity;
- artifact size limits;
- generated artifact provenance;
- generator and validator command shape;
- generator output and validator input coverage;
- structured input references to declared repos;
- tracked generated files and ignored output directories.

### `vo-dev lint repo-boundaries`

Checks:

- raw sibling repo references are forbidden outside declared policy;
- CI checkout paths come from `eng/project.toml`;
- first-party Node workspace paths match project workspaces;
- tasks that use `vo.work` declare it.

### `vo-dev lint release`

Checks:

- target slug/path shapes;
- package names and artifact prefixes;
- release profile and LTO policy;
- package build args;
- Homebrew metadata;
- checksum/tarball expectations.

### `vo-dev test lint`

Checks:

- manifest schema;
- duplicate test ids;
- missing files;
- unlisted test files/projects/archives;
- blank unmarked files;
- expected failures without diagnostics;
- skipped targets without reasons;
- target names not declared in `eng/tests.toml`;
- required coverage target omissions.

### `vo-dev lint layout`

Checks:

- no tracked files under `.examples`;
- no command crate root under `cmd/*/rust`;
- no unapproved root files;
- no unapproved top-level hidden state directories;
- no tracked local build outputs such as root `.vob` files;
- no app root outside the declared app layout;
- no executable test corpus files directly under the `tests/lang` root.

### `vo-dev lint docs`

Checks:

- generated doc mirrors match source digest;
- no manually edited spec mirror exists outside `lang/docs/spec`;
- dev-note front matter is present for new or touched notes;
- superseded notes link to replacements.

### `vo-dev lint examples`

Checks:

- every example source file is listed in `examples/manifest.toml`;
- every manifest path exists;
- smokeable examples have task coverage;
- hidden or ignored example directories are rejected.

### `vo-dev lint benchmarks`

Checks:

- every benchmark directory is declared in `benchmarks/manifest.toml`;
- every manifest entry has tools and outputs;
- generated results are ignored or covered by artifact policy.

---

## 10. Direct Cutover Steps

Each step may be implemented separately, but an accepted step leaves its surface in the final layout.
No step is complete if it leaves an old source path active beside the new one.

### Step C0: Engineering Control Plane

Implementation:

- add `eng/README.md`, `eng/tasks.toml`, `eng/toolchains.toml`, `eng/tests.toml`, `eng/artifacts.toml`, `eng/ci.toml`, `eng/project.toml`, and `eng/release.toml`;
- add `cmd/vo-dev`;
- move CI planning and task execution into `vo-dev`;
- move release logic into `vo-dev release`;
- make `d.py` a thin facade;
- delete `d_py.py`;
- delete Python CI planning/execution scripts.

Acceptance:

- `cargo run -p vo-dev -- task list` lists the task surface.
- `cargo run -p vo-dev -- task plan pr --changed --format json` includes local untracked files.
- GitHub workflows obtain matrix/metadata from `vo-dev`.
- `./d.py ci smart` delegates to `vo-dev`.
- `./d.py test both` delegates to `vo-dev test run`.

### Step C1: Test Planning

Implementation:

- create `eng/tests.toml`;
- create `tests/lang/manifest.toml`;
- implement `vo-dev test lint|plan|run`;
- implement `vo-test run-plan`;
- update test tasks to call `vo-dev test run`;
- delete old runner mode selection.

Acceptance:

- generated plans cover VM, JIT, GC VM/JIT, OSR, no_std/embed, WASM, and compile diagnostics.
- GC jobs contain explicit `VO_GC_DEBUG`.
- OSR jobs contain explicit JIT threshold env.
- expected-failure jobs verify diagnostics.
- `vo-test run-plan` executes without walking the test tree.

### Step C2: Test Corpus Physical Move

Implementation:

- create `tests/lang/cases`, `tests/lang/projects`, `tests/lang/archives`, and `tests/lang/fixtures`;
- move files from `lang/test_data` with `git mv`;
- update manifest root and paths;
- update docs, skills, task inputs, and focused-test examples;
- delete old-path handling for `lang/test_data` in the same change.

Acceptance:

- language test plans contain `tests/lang` paths.
- no executable test source remains under `tests/lang`.
- old-path remapping does not exist.

### Step C3: Artifact And Repo Boundary Policy

Implementation:

- wire `vo-dev lint artifacts` into quality;
- wire `vo-dev lint repo-boundaries` into quality;
- declare quickplay package policy and provenance;
- declare first-party repos and workspaces;
- move root generic fixtures into owner fixture directories.

Acceptance:

- adding tracked `.wasm`, large `.json`, `dist`, or `pkg` without artifact policy fails CI.
- tasks cannot reference undeclared sibling repos.
- quickplay validation rejects stale provenance, dirty external-source provenance, output digest mismatches, missing dependency artifact digests, or stale module versions.

### Step C4: Docs Single Source

Implementation:

- add docs source manifest or generator;
- remove Playground spec mirror;
- generate Playground docs under `apps/playground-legacy/src/assets/docs/generated`;
- add `apps/studio/docs/manifest.toml`;
- add `docs-lint` and wire it into quality.

Acceptance:

- app-visible specs cannot diverge silently from `lang/docs/spec`.
- app docs distinguish help/tutorial content from language spec content.

### Step C5: Examples And Root Hygiene

Implementation:

- move `examples/*.vo` into `examples`;
- add `examples/manifest.toml`;
- remove `/examples/` from `.gitignore`;
- move or delete `hello.vo`;
- ensure `main.vob` is ignored, cleaned, and not tracked.

Acceptance:

- `git ls-files .examples` prints nothing.
- `vo-dev lint examples` passes.
- `vo-dev lint layout` has no root scratch-file findings.

### Step C6: Command Crate Normalization

Implementation:

- move `cmd/vo-test/rust/Cargo.toml` to `cmd/vo-test/Cargo.toml`;
- move `cmd/vo-test/rust/src` to `cmd/vo-test/src`;
- update workspace members, package references, docs, and tasks;
- remove old Vo runner remnants from `cmd/vo-test`.

Acceptance:

- `cargo check -p vo-test` passes.
- no current docs instruct users to edit `cmd/vo-test`.

### Step C7: Local State Consolidation

Implementation:

- update `vo-dev`, Studio launch, and repo scripts to use `.volang`;
- add `.volang/` to `.gitignore`;
- declare local-state policy in `eng/artifacts.toml` or layout lint data;
- remove old local-state paths.

Acceptance:

- normal repo commands do not create `.volang/studio/sessions`, `.volang/studio/sources`, or repo-root `.vo-cache`.
- clean commands remove `.volang` state by class.

### Step C8: App Layout Move

Implementation:

- rename tasks and docs from `playground` to `playground-legacy`;
- move `playground` to `apps/playground-legacy`;
- move `studio` to `apps/studio`;
- update Node workspaces, Cargo members, task inputs/outputs, workflows, and docs.

Acceptance:

- app tasks run from `apps/*`.
- no top-level `studio` or `playground` app roots remain.

### Step C9: Benchmark Manifest And Docs Lifecycle

Implementation:

- add `benchmarks/manifest.toml`;
- update `vo-dev bench` to read it;
- add dev-note front matter lint;
- move `lang/docs/outdated` to `lang/docs/archive` or add lifecycle metadata.

Acceptance:

- benchmark discovery is manifest-driven.
- new or touched historical docs cannot be added without status metadata.

---

## 11. Precise Acceptance Checklist

The target state is accepted only when:

1. `cargo run -p vo-dev -- task list` lists every runnable task.
2. `cargo run -p vo-dev -- lint all` runs task, artifact, repo-boundary, release, layout, docs, examples, and benchmark lints.
3. `cargo run -p vo-dev -- tool check --task <task>` reads all required tool versions from `eng/toolchains.toml`.
4. `cargo run -p vo-dev -- test lint --suite lang` passes with `root = "tests/lang"`.
5. `cargo run -p vo-dev -- test plan --suite lang --targets vm,jit --format json` emits only `tests/lang` test paths.
6. `vo-test run-plan` can execute a generated plan without filesystem discovery.
7. `.github/workflows/*` get task matrix/metadata/release data from `vo-dev`.
8. no workflow directly runs a build/test command for a task declared in `eng/tasks.toml`.
9. `./d.py` contains dispatch only.
10. `scripts/ci` contains only narrow helper scripts.
11. root `Cargo.toml` uses `cmd/vo-test`, not `cmd/vo-test/rust`.
12. `git ls-files lang/test_data .examples` prints nothing.
13. app roots are `apps/studio` and `apps/playground-legacy`.
14. generated Playground docs are fresh from `lang/docs`.
15. Studio docs under `apps/studio/docs` do not fork language spec content.
16. root scratch files and bytecode outputs are absent.
17. local Volang state is under `.volang`.
18. `benchmarks/manifest.toml` drives benchmark discovery.
19. every checked-in generated artifact has declared policy and provenance where required.
20. every touched design note has lifecycle metadata.

---

## 12. Non-Goals

This design does not change:

- Volang syntax;
- type semantics;
- bytecode format;
- runtime semantics;
- GC algorithm;
- JIT behavior;
- module resolution semantics;
- standard library behavior;
- the public `vo` CLI contract for arbitrary user projects;
- release versioning;
- whether Studio remains Svelte/Tauri;
- whether Playground remains available as a legacy app;
- the first-party multi-repository strategy.

This design also does not require every physical move in the repository to happen in one patch.
It does require each implemented surface to cut over directly: one source root, no active old-path state, and lint coverage for the new boundary.

---

## 13. Design Decisions

### Decision D1: The Control-Plane CLI Is Rust

Reason:

- the repo already requires Rust;
- Rust avoids Python version drift;
- Rust gives typed TOML/JSON schemas and fast path matching;
- `vo-dev` can be tested with normal Cargo tests.

### Decision D2: `./d.py` Remains A Facade

Reason:

- existing developer muscle memory remains valid;
- external scripts can call `./d.py`;
- command ownership remains centralized in `vo-dev`.

End state:

- `./d.py` contains dispatch only;
- operational logic lives in `vo-dev`.

### Decision D3: Test Discovery Moves Out Of `vo-test`

Reason:

- target policy, skips, expected failures, and runtime env are planning concerns;
- execution should be a pure plan runner;
- native and WASM tests can share one logical test plan.

### Decision D4: Quickplay Is Explicit Policy, Not An Exception

Reason:

- checked-in static payloads need provenance;
- deployment behavior must be reproducible;
- if deployment no longer needs committed static payloads, quickplay can become a release-output artifact.

### Decision D5: App Layout Uses `apps/*`

Reason:

- Studio and Playground are apps, not language crates;
- Playground's legacy status must be visible in paths and task names;
- app docs and generated docs should live under the owning app root.

### Decision D6: No Completed Surface Keeps Two Source Roots

Reason:

- dual roots are the main source of repo confusion;
- lints can only enforce ownership when the ownership is singular;
- physical layout should communicate source truth without tribal knowledge.
