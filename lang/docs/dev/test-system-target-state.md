# Volang Test System Target State

Status: target-state design implemented in the `vo-dev` control plane.

Scope: the whole repository validation system, including language regression
tests, Rust unit and contract tests, GC/JIT correctness checks, WASM execution,
examples, benchmarks, docs policy, Studio and quickplay smoke checks, release
verification, and intentionally excluded legacy surfaces.

Detailed execution plan:
`lang/docs/dev/test-system-completion-plan.md`.

The executable source of truth remains `eng/*.toml`, `cmd/vo-dev`,
`tests/lang/manifest.toml`, `cmd/vo-test`, and the declared task scripts. This
document describes the shape those sources implement. It must not become a
second copy of test policy.

## Implementation Status

The target state is represented in executable policy:

- `eng/tests.toml` defines named matrices for all language target patterns.
- `tests/lang/manifest.toml` cases carry complete matrix, tag, owner, and
  expectation metadata.
- Strict language lint and coverage commands reject missing metadata, target
  arrays, unowned skips/failures/timeouts, and filename-only GC selection.
- Native and WASM language runs emit `volang.test-result.v1`, and
  `vo-dev test run --format json` aggregates mixed native/WASM output.
- `eng/tasks.toml` declares first-class group metadata plus owner/tag/tier
  metadata for every task.
- Contract, stress, app/site, docs, release, example, benchmark, and
  legacy-excluded surfaces are visible in task stats and task coverage.
- `vo-dev task plan ... --explain`, `vo-dev test plan ... --explain`, and
  `vo-dev test explain --suite lang --case <id>` explain task and case
  selection.

Use `lang/docs/dev/test-authoring-guide.md` and
`lang/docs/dev/test-ci-routing-guide.md` when adding or debugging coverage.

## Goal

Volang should have one explainable testing control plane where every validation
surface answers these questions:

- What behavior or contract does this test protect?
- Which subsystem owns it?
- Which runner executes it?
- Which targets or backends does it cover?
- Which tier decides when it runs?
- Which result schema reports it?
- Why did CI select it for this change?

The target state is not a new standalone framework. It is a clearer taxonomy and
metadata model around the framework that already exists.

## Non-Goals

- Do not add a parallel shell-script or YAML-defined test graph.
- Do not move all language test files before adding metadata. Large file moves
  should come after the test catalog is clear.
- Do not replace focused Rust unit tests with manifest cases, or replace
  manifest language regressions with crate tests. They prove different things.
- Do not make GC correctness a separate runner. GC correctness should be a
  first-class contract layer inside the existing control plane.
- Do not make `d.py` or GitHub workflow YAML interpret test policy. They should
  continue to delegate to `vo-dev`.

## Current Surfaces

The target state should explicitly account for these current surfaces.

| Surface | Current owner file or runner | Role |
| --- | --- | --- |
| Language case selection | `tests/lang/manifest.toml`, `cmd/vo-dev/src/test_*` | Select `.vo`, project, and zip cases. |
| Native language execution | `cmd/vo-test` | Execute VM/JIT/OSR/nostd/native compile jobs and compare backend outputs. |
| WASM language execution | `lang/crates/vo-web/test_runner.mjs` | Execute language cases through `vo-web` in Node/WASM. |
| Language target policy | `eng/tests.toml` | Define targets, aliases, default selections, and target env. |
| Repo task graph | `eng/tasks.toml`, `cmd/vo-dev/src/task_*` | Define task groups, dependencies, inputs, outputs, tools, and tiers. |
| Smart CI routing | `eng/ci.toml`, `cmd/vo-dev/src/ci_system.rs`, `cmd/vo-dev/src/task_planner.rs` | Select tasks from changed paths. |
| Rust crate tests | `#[test]` and crate `tests/` directories | Unit, integration, model, and contract coverage for implementation internals. |
| GC contract tests | `gc-contract` task group and GC-focused crate tests | Protect runtime/VM/JIT GC invariants. |
| Examples and benchmarks | `examples/manifest.toml`, `benchmarks/manifest.toml`, lint tasks | Keep public examples and benchmark metadata synchronized. |
| Docs and generated docs | `scripts/ci/docs_lint.mjs`, `scripts/ci/docs_sync.mjs`, Studio docs manifest | Verify source-backed docs and generated mirrors. |
| Studio and quickplay | Studio npm scripts, quickplay validators, BlockKart smoke scripts | Build and smoke-check app-visible artifacts. |
| Release verification | `eng/release.toml`, release verify tasks | Validate release package policy. |
| Legacy excluded surface | `apps/playground-legacy/rust` / `vo-playground` | Exists in the workspace but is excluded from main cargo gates. |

## Core Principles

1. One control plane.
   `vo-dev` interprets repo test policy. `d.py`, workflow YAML, and shell
   scripts remain facades or task bodies.

2. Orthogonal axes.
   Input kind, execution target, subsystem domain, owner, tier, and expectation
   should be separate fields. No single field should carry all meaning.

3. Selection must be explainable.
   A developer should be able to ask why a task or case was selected and get a
   concrete answer based on changed paths, manifest metadata, matrix policy, or
   explicit CLI filters.

4. Runners execute; planners decide.
   `cmd/vo-test` and the WASM runner should execute resolved jobs. Selection,
   target expansion, linting, and mixed-result aggregation should live in
   `vo-dev`.

5. Contract tests are first-class.
   GC, JIT, bytecode metadata, module, release artifact, and runtime ABI checks
   should be visible as contract tests, not hidden inside generic `test`.

6. Metadata before movement.
   The catalog should become clear before large directory reshuffles. Directory
   layout should eventually reflect metadata, not substitute for it.

7. Every skip and expected failure has a reason.
   Skips and expected failures are acceptable only when they remain visible and
   owned.

## Target Taxonomy

Every validation item should fit into these axes.

### Surface

The broad family of validation:

- `lang-case`: manifest-driven `.vo`, project, or zip language regression.
- `crate-unit`: Rust unit tests for local implementation behavior.
- `crate-integration`: Rust integration tests spanning crate boundaries.
- `contract`: cross-layer invariant tests, such as GC root correctness, JIT
  semantic facts, metadata layout, ABI helper signatures, and release artifact
  rules.
- `model`: small model tests for invariants that are hard to exhaust through
  user programs.
- `perf-smoke`: bounded performance sanity checks, not benchmark scoring.
- `example-smoke`: public example compile or run smoke checks.
- `manifest-lint`: examples, benchmarks, docs, language case, or artifact
  catalog validation.
- `docs-policy`: source-backed docs and generated docs mirror checks.
- `app-build`: Studio, vo-web, and app WASM build checks.
- `app-smoke`: quickplay and BlockKart local or remote smoke checks.
- `release-verify`: release package policy validation.
- `legacy-excluded`: known surface that exists but is not part of the main gate.

### Input Kind

For language cases only:

- `file`: a single `.vo` input.
- `project`: a project directory with `vo.mod`.
- `zip`: an archive input.

The existing manifest field `kind` currently means this physical input shape.
Target state may keep `kind` for compatibility, but docs and lint errors should
call it input kind. If a future schema can afford a compatibility migration,
`input_kind` is clearer.

### Matrix

A matrix is a named set of execution targets. It should describe policy, not
test semantics.

Target matrix examples:

```toml
[matrices.default]
targets = ["vm", "jit", "osr", "nostd", "wasm"]

[matrices.native]
targets = ["vm", "jit", "osr"]

[matrices.gc]
targets = ["gc-vm", "gc-jit"]

[matrices.compile]
targets = ["compile"]
```

`targets` should remain available as an explicit override. The common case
should use `matrix`, not repeated target arrays.

### Target

A target is a concrete execution mode:

- `compile`: native compile-fail or compile-only behavior.
- `vm`: native VM execution.
- `jit`: native JIT execution.
- `osr`: OSR execution.
- `nostd`: `vo-embed` style no-stdlib/native-embed execution.
- `wasm`: `vo-web` execution through the WASM runner.
- `gc-vm`: VM execution with GC stress and verification env.
- `gc-jit`: JIT execution with GC stress, verification env, and low JIT
  threshold.

Targets belong in `eng/tests.toml`. Target environments should stay there.

### Domain Tags

Tags describe what the case or task protects:

- Runtime domains: `gc`, `scheduler`, `panic`, `defer`, `interface`, `slice`,
  `map`, `channel`, `island`, `dyn`, `stdlib`, `module`, `ffi`.
- Compiler domains: `parser`, `typechecker`, `codegen`, `bytecode`,
  `metadata`, `jit`, `osr`.
- Policy domains: `docs`, `artifact`, `release`, `quickplay`, `studio`,
  `example`, `benchmark`.
- Intent tags: `regression`, `contract`, `smoke`, `model`, `perf`,
  `compile-fail`.

Tags should be used for selection, reporting, ownership dashboards, and lint
rules. They should not replace target matrices.

### Owner

Every non-trivial case, task, or manifest category should have an owner string.
Examples:

- `runtime-gc`
- `jit`
- `vm`
- `codegen`
- `typechecker`
- `stdlib`
- `module`
- `web-wasm`
- `studio`
- `release`
- `eng`

Owners are not necessarily people. They are stable subsystem names for routing.

### Tier

Tiers describe cadence and cost.

| Tier | Meaning |
| --- | --- |
| `fast` | Cheap policy, fmt, check, lint, and smoke checks used for early feedback. |
| `test` | Main functional tests expected in normal PR validation. |
| `contract` | Cross-layer invariants that prevent high-impact regressions. |
| `stress` | Heavier stress, GC stress, fuzz-like, repeat, or long-running checks. |
| `site` | Studio, web, quickplay, and deployed app validation. |
| `release` | Release package and publication policy verification. |
| `manual` | Explicitly generated artifacts or local-only maintenance tasks. |
| `legacy` | Known excluded or transitional surfaces. |

The current task lint should allow at least `contract` and `stress` in addition
to existing tiers. GC contract tasks should not be hidden under only `test`.

## Target Language Manifest Shape

The language manifest should describe case semantics directly and leave target
expansion to `eng/tests.toml`.

Default functional case:

```toml
[[case]]
id = "slice-append-semantics"
kind = "file"
path = "cases/slice_append_semantics.vo"
matrix = "default"
tags = ["slice", "runtime", "regression"]
owner = "runtime"
expect = "pass"
```

GC regression case:

```toml
[[case]]
id = "gc-slice-grow-retains-elements"
kind = "file"
path = "cases/gc_slice_grow.vo"
matrix = "gc"
tags = ["gc", "slice", "barrier", "regression"]
owner = "runtime-gc"
expect = "pass"
```

Compile-fail case:

```toml
[[case]]
id = "typechecker-invalid-pointer-receiver"
kind = "file"
path = "cases/typechecker/invalid_pointer_receiver.vo"
matrix = "compile"
tags = ["typechecker", "compile-fail"]
owner = "typechecker"
reason = "documents pointer receiver validation"
expect = { fail = ["invalid pointer type"] }
```

Override case:

```toml
[[case]]
id = "blocking-file-ops"
kind = "file"
path = "cases/blocking_file_ops.vo"
matrix = "default"
skip = ["nostd", "wasm"]
reason = "requires native file APIs not available in those targets"
tags = ["stdlib", "os", "regression"]
owner = "stdlib"
expect = "pass"
```

Rules:

- `matrix` is required for new pass cases.
- `targets` is allowed only for exceptional overrides, and lint should explain
  why `matrix` is preferred.
- `tags` and `owner` are required for new cases.
- `reason` is required for every `skip`, expected failure, timeout exception,
  or target override.
- GC-looking cases should be selected by `tags = ["gc"]` or `matrix = "gc"`,
  not filename heuristics.
- `vo-dev test lint --suite lang` should validate the catalog and report
  counts by matrix, target, tag, owner, skip, and expected failure.

## Target Task Graph Shape

Task groups should describe product lines and cadences, not just long command
lists.

Target groups:

- `quality`: source policy, layout, docs policy, fmt, clippy, check, and quick
  compatibility smoke.
- `lang-main`: default language regression and compile-fail coverage.
- `lang-backends`: OSR, nostd, and WASM language coverage.
- `gc-contract`: runtime, VM, JIT, layout, root scanning, and GC model
  contracts.
- `jit-contract`: semantic rows, verifier, metadata, ABI helper, callback, and
  materialization contracts.
- `runtime-contract`: non-GC runtime invariants such as dynamic values, pack,
  scheduler, and panic/defer contracts.
- `examples`: examples manifest lint and smoke checks.
- `docs`: docs lint and generated mirror validation.
- `app-site`: vo-web build, Studio WASM build, Studio build, quickplay
  validation, and local app smoke.
- `release-verify`: release package policy checks.
- `test`: main PR functional gate; should include `lang-main`, selected
  contract groups, examples, and release cargo tests.
- `contract`: full contract layer; can be required by smart CI for risky paths
  and by nightly/full runs.
- `site`: app and web validation.
- `full`: all normal validation except explicitly manual tasks.
- `nightly`: optional future group for stress, repeats, fuzz-like checks, and
  heavy perf smoke.

Every task should declare:

- `name`
- `title`
- `command`
- `tools`
- `inputs`
- `outputs`
- `tier`
- `domain` or `tags`
- `owner`
- `needs`
- `internal` when the task is not user-facing

`eng-lint-tasks` should enforce the metadata for new tasks.

## Result Model

The target result model has one schema for all language job execution.

Native and WASM language runs should both report:

- suite
- plan schema version
- result schema version
- job id
- case id
- target
- matrix
- tags
- owner
- expectation
- status
- stdout/stderr or normalized diagnostic summary
- duration
- skip or fail reason
- differential baseline when applicable

`cmd/vo-test` already owns native execution result reporting. The WASM runner
should become a compatible adapter that emits the same result schema. `vo-dev`
should aggregate mixed native/WASM results.

Target commands:

```sh
cargo run -q -p vo-dev -- test catalog --suite lang --format json
cargo run -q -p vo-dev -- test stats --suite lang
cargo run -q -p vo-dev -- test plan --suite lang --matrix gc
cargo run -q -p vo-dev -- test run --suite lang --tags gc --format json
```

These commands are implemented through `vo-dev`. The remaining result-schema
work is native/WASM result aggregation, not catalog or case selection.

## CI Routing Target State

Smart CI should be explainable at two levels:

1. Task selection: why a task was selected for a changed path.
2. Case selection: why a language case or matrix was selected.

Examples:

- `lang/crates/vo-runtime/src/gc.rs` selects `gc-contract`,
  `runtime-contract`, `vo-test-gc`, and relevant cargo tests because it matches
  `domain = ["gc", "runtime"]`.
- `lang/crates/vo-jit/src/semantics/**` selects `jit-contract`, JIT crate
  tests, and language JIT/OSR smoke because it matches `domain = ["jit"]`.
- `tests/lang/cases/gc_slice_grow.vo` selects language manifest lint and the
  case's declared matrix because it is a manifest input.
- `apps/studio/src/**` selects Studio build and site smoke, not language GC
  tests unless shared runtime inputs changed.

Target command:

```sh
cargo run -q -p vo-dev -- task plan pr --changed --explain
```

The explanation should name the matched path rule, task input, domain tag, or
fallback rule.

## GC Correctness Target State

GC correctness should be a contract layer made of several complementary parts.

Language regressions:

- `matrix = "gc"` expands to `gc-vm` and `gc-jit`.
- GC cases carry `tags = ["gc", ...]` and `owner = "runtime-gc"`.
- GC stress env remains target policy in `eng/tests.toml`.

Crate contracts:

- Runtime GC tests protect allocator, marking, sweeping, root table, object
  metadata, write barriers, and debug verification.
- VM GC tests protect root scanning, stack slots, interface slots, defer/panic
  paths, scheduler boundaries, and layout validation.
- JIT GC tests protect frame materialization, spills, side exits, VM-call
  materialization, callback ABI, helper may-GC policy, and hidden roots.
- JIT semantic tests protect metadata requirements and verifier rules that make
  GC scanning sound.

Model and stress:

- Small model tests should cover root graph and object liveness invariants that
  are hard to express through `.vo` programs.
- Stress tests should be explicit `tier = "stress"` or `tier = "contract"`
  depending on cost. They should not hide under default `test` if they are
  expensive.

The desired result is not that GC bugs become impossible. The desired result is
that a GC-sensitive change automatically selects the right contract surface and
that each missed bug can be added to a named layer with an owner.

## App, Docs, and Release Target State

The test system should treat non-language validation as first-class.

Docs:

- `docs-sync` is a generator.
- `docs-lint` is a gate that validates source-backed docs and generated mirrors.
- Generated artifacts remain governed by `eng/artifacts.toml`.

Studio and quickplay:

- `studio-wasm-build`, `studio-build`, `quickplay-validate`, and
  `blockkart-smoke-static` belong to `app-site`.
- `blockkart-smoke-remote` belongs to deployed site smoke and should remain
  separate from local PR checks unless explicitly requested.

Release:

- Release verify tasks belong to `release-verify`.
- Release tasks should not be mixed with language functional tests except in
  `full` or release-specific gates.

Legacy:

- `vo-playground` should be explicitly classified as `legacy-excluded` while it
  remains excluded from main cargo gates.
- If it becomes important again, it should either join `app-site` with a clear
  task or be removed from the workspace. The current state should not remain
  implicit.

## Directory Layout Target

Directory movement is useful only after metadata lands.

Preferred eventual language case layout:

```text
tests/lang/cases/core/
tests/lang/cases/gc/
tests/lang/cases/jit/
tests/lang/cases/stdlib/
tests/lang/cases/typechecker/
tests/lang/cases/dyn/
tests/lang/cases/module/
tests/lang/cases/platform/
tests/lang/cases/bugs/
```

Rules:

- The manifest remains the authority even after directories are cleaned up.
- Directories provide a human browsing aid, not selection policy.
- Moving files should preserve case ids and be done after catalog/stats tooling
  can verify that coverage did not change.

## Migration Plan

### Phase 0: Inventory Lock

- Implemented: `vo-dev test stats --suite lang`.
- Implemented: `vo-dev test catalog --suite lang --format json`.
- Implemented: `vo-dev task stats`.
- Current stats expose language cases, jobs, skips, expected failures,
  matrices, targets, tags, owners, task tiers, and group sizes.

### Phase 1: Metadata Schema

- Implemented: `eng/tests.toml` has named matrices.
- Implemented: language manifest parsing accepts `matrix`, `tags`, and `owner`.
- Implemented: strict completion lint rejects explicit language `targets`.
- Implemented: `vo-dev test plan|run` filters by `--matrix`, `--tags`, and
  `--owner`.
- Implemented: task lint accepts and enforces `contract` and `stress` tiers.
- Implemented: task `tags`, `owner`, and group metadata are required by strict
  lint.

### Phase 2: Manifest Normalization

- Implemented: all language cases declare `matrix`, `tags`, `owner`, and
  `expect`.
- Implemented: compile-fail cases use `matrix = "compile"` and
  `tags = ["compile-fail", ...]`.
- Implemented: GC cases are selected by matrix/tag metadata, not filenames.
- Preserved: case ids, paths, expectations, skips, and resolved job counts.
- Implemented: strict lint rejects target arrays and incomplete metadata.

### Phase 3: Contract Layer

- Implemented: `contract` and `stress` task tiers are accepted.
- Implemented: GC, JIT, runtime, typechecker, codegen, module, release, docs,
  and app contract groups are visible and owned.
- Implemented: top-level `contract`, `stress`, `test`, `pr`, and `full`
  groups compose first-class group metadata.

### Phase 4: Unified Results

- Implemented: the WASM runner emits the same `volang.test-result.v1` schema as
  `cmd/vo-test`.
- Implemented: `vo-dev test run --format json` aggregates native and WASM jobs.
- Implemented: result jobs include matrix, tags, owner, expectation, failure
  reason, artifacts, and differential baseline metadata.

### Phase 5: Explainable CI

- Implemented: `vo-dev task plan ... --explain`.
- Implemented: task planning reports matched known prefixes, task inputs,
  dependency expansion, downstream expansion, and fallback reasons.
- Implemented: focused path rules cover GC, JIT, runtime, codegen, module, and
  app/site surfaces.

### Phase 6: Directory Cleanup

- Move language cases into clearer domain directories after metadata coverage is
  stable.
- Keep the manifest as the authoritative catalog.
- Use catalog diffs to confirm no jobs were lost during movement.

### Phase 7: Optional New Testing Techniques

After the taxonomy is stable, consider adding focused tools only where they fit
an existing surface:

- Property/model testing for GC root graph invariants.
- Snapshot testing for diagnostics if compiler diagnostics stabilize enough.
- Fuzz-like stress for parser/typechecker/runtime boundaries.
- Perf smoke or benchmark comparison for known hot paths.

These should not be introduced as a broad new framework before the control
plane is clear.

## Success Criteria

The target state is reached when:

- A new language case can be added without repeating default target arrays.
- Every language case has matrix, tags, owner, and expectation.
- Every skip, expected failure, timeout exception, or target override has a
  reason.
- GC cases are selected by metadata, not filename heuristics.
- GC/JIT/runtime contract tests are visible as `contract`, not hidden in
  generic `test`.
- Native and WASM language runs can produce one result schema.
- `vo-dev` can print catalog and stats reports for cases and tasks.
- Smart CI can explain why each task was selected.
- App/site, docs, release, and legacy-excluded surfaces are visible in the same
  taxonomy as language and Rust tests.
- `d.py`, workflow YAML, and shell scripts do not duplicate policy.

## Review Checklist For Future Changes

For any new test or validation task:

- Does it have the correct surface?
- Does it have an owner?
- Does it have domain tags?
- Does it belong to the right tier?
- Is selection expressed in `eng/*.toml` or manifest metadata rather than ad hoc
  script logic?
- Does the runner only execute resolved work?
- Does `vo-dev` have enough metadata to explain selection?
- Does the result fit an existing schema?
- If this is GC-sensitive, does it hit both language GC matrix coverage and the
  relevant crate contract layer?
