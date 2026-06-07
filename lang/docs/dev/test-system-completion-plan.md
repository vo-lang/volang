# Volang Test System Completion Plan

Status: completion checklist implemented; keep this as the acceptance contract
for future regressions.

Goal: after this plan is complete, the Volang test system should have no
obvious structural improvement left. Future work should mostly add product
coverage for new language/runtime features, not repair the test system itself.

This plan is deliberately stricter than
`lang/docs/dev/test-system-target-state.md`. The target-state document describes
the shape. This document defines the work, hard gates, migration order, and
acceptance checks required before calling the test system complete.

## Completion Standard

The test system is complete only when all of these are true:

- Every validation surface is represented in one `vo-dev` control plane.
- Every language case has a matrix, tags, owner, expectation, and stable path.
- Every skip, expected failure, timeout, target override, and legacy exclusion
  has a reason.
- Every public task has tier, owner, tags, tools, inputs, outputs, and group
  membership.
- Every task group has a documented role, owner, tier/cadence intent, and
  inclusion policy for PR, full, site, release, contract, and stress runs.
- Native and WASM language execution emit one result schema.
- Mixed native/WASM `vo-dev test run --format json` is supported.
- CI planning can explain every selected task and every fallback.
- Case selection can explain every selected language case and target.
- Contract coverage exists for GC, JIT, runtime, typechecker, codegen,
  bytecode metadata, module, release artifacts, docs, examples, and app/site
  validation.
- Strict lints fail if new tests or tasks bypass the taxonomy.
- Directory layout matches the metadata catalog and no longer carries hidden
  selection meaning.
- Full validation can be run from declared `vo-dev` tasks without policy hidden
  in workflow YAML, shell snippets, or `d.py`.

The final state should make the natural answer to "where does this test belong"
obvious from the manifest or task metadata, not from maintainer memory.

## Starting Baseline

The gap counts in this section describe the pre-completion baseline that this
plan closed. Current counts should be read from:

```sh
cargo run -q -p vo-dev -- test coverage --suite lang --format json
cargo run -q -p vo-dev -- task coverage --format json
cargo run -q -p vo-dev -- test stats --suite lang --format json
cargo run -q -p vo-dev -- task stats --format json
```

Current control-plane seed:

- `eng/tests.toml` has named matrices.
- `tests/lang/manifest.toml` accepts `matrix`, `tags`, and `owner`.
- `vo-dev test catalog`, `vo-dev test stats`, and `vo-dev task stats` exist.
- `vo-dev test plan|run` can filter by matrix, tags, owner, and path.
- Native `vo-test` JSON results carry matrix, tags, and owner.
- GC contract tasks are visible as `tier = "contract"`.

Current gaps from `vo-dev test stats --suite lang`:

- Language cases: 1068.
- Cases still using explicit targets: 1054.
- Cases without matrix: 1067 total if explicit-target cases and compile-fail
  cases are counted together.
- Cases without tags: 1065.
- Cases without owner: 1065.
- Expected-fail cases: 12.
- Skip entries: 209.

Current gaps from `vo-dev task stats`:

- Tasks: 44.
- Public and internal tasks missing owner: 39.
- Tasks missing tags: 39.
- Contract tier currently covers GC only.
- Group metadata does not exist as first-class data.

This baseline is useful infrastructure, not completion.

## Hard Rules

These rules are not optional cleanup. They are what prevent the system from
regressing into a half-finished taxonomy.

### Language Case Rules

Each `[[case]]` in `tests/lang/manifest.toml` must have:

- `id`
- `kind`
- `path`
- `matrix`
- `tags`
- `owner`
- `expect`

Allowed exceptions:

- `targets` may appear only with a `reason` that explains why no named matrix
  fits.
- `skip` requires `reason`.
- `timeout` requires `reason`.
- `expect = { fail = ... }` requires `matrix = "compile"` and a `reason`.
- `blank = true` cases must be explicitly skipped and owned by `eng` or the
  relevant subsystem.
- Truly legacy cases must use `tags = ["legacy"]` and an owner, not missing
  metadata.

Lint must reject:

- Missing `matrix`, `tags`, or `owner`.
- Empty or duplicate tags.
- Uppercase or whitespace in matrix, tag, or owner names.
- Runtime targets on compile-fail cases.
- Filename-inferred GC selection when `tags` or `matrix` are missing.
- Explicit target arrays that match a named matrix.
- Explicit target arrays without a reason.
- Skip or timeout entries for targets not selected by the resolved matrix.

### Task Rules

Every `[[task]]` in `eng/tasks.toml` must have:

- `name`
- `title`
- `command`
- `tools`
- `inputs`
- `outputs`
- `tier`
- `tags`
- `owner`
- `needs` when ordering matters
- `internal = true` when not user-facing

Lint must reject:

- Missing owner or tags.
- Invalid tier.
- Public tasks not included in a relevant group.
- Groups referencing unknown tasks.
- Tasks with broad inputs when narrower subsystem inputs are available.
- Tasks that invoke test policy outside `vo-dev`, except as declared task
  bodies.
- `contract` or `stress` tasks without explicit owner and domain tags.

### Group Rules

Task groups must become first-class, not just arrays. Each group should have:

- `name`
- `title`
- `tier_intent`
- `owner`
- `tags`
- `tasks`
- `included_in`
- `selection_policy`

Compatibility can keep the existing `[groups]` arrays during migration, but
completion requires metadata for each group.

Required groups:

- `quality`
- `lang-main`
- `lang-backends`
- `compile-contract`
- `gc-contract`
- `jit-contract`
- `runtime-contract`
- `typechecker-contract`
- `codegen-contract`
- `module-contract`
- `docs`
- `examples`
- `benchmarks`
- `app-site`
- `release-verify`
- `legacy-excluded`
- `contract`
- `stress`
- `test`
- `site`
- `full`
- `pr`

### Result Schema Rules

All language execution results must use one schema, regardless of target.

Required per-result fields:

- `schema`
- `suite`
- `passed`
- `failed`
- `skipped`
- `jobs`

Required per-job fields:

- `id`
- `case_id`
- `kind`
- `path`
- `target`
- `backend`
- `matrix`
- `tags`
- `owner`
- `expect`
- `status`
- `elapsed_ms`
- `stdout`
- `stderr`
- `error`
- `skip_reason`
- `failure_reason`
- `baseline`
- `artifacts`

`baseline` should identify differential comparison source when applicable:

- `jit` compared with `vm`
- `osr` compared with `vm`
- `gc-jit` compared with `gc-vm` when available
- `gc-jit` compared with `vm` only when `gc-vm` is not selected

WASM may normalize diagnostics, but it must not use a separate shape.

## Work Package 1: Schema And Strict Lints

Objective: make it impossible to add new half-metadata tests or tasks.

Implementation work:

- Add strict mode to `vo-dev test lint --suite lang --strict`.
- Add strict mode to `vo-dev lint tasks --strict`.
- Add `vo-dev test coverage --suite lang` for metadata coverage, not code
  coverage.
- Add `vo-dev task coverage` for task/group metadata coverage.
- Add JSON output for test and task stats.
- Add golden schema tests for catalog, plan, result, stats, and task plan JSON.
- Add lint messages that include the exact suggested metadata shape.
- Add compatibility warnings for explicit target arrays that match matrices.
- Add a documented allowlist for temporary migration exceptions.

Acceptance gates:

```sh
cargo run -q -p vo-dev -- test lint --suite lang
cargo run -q -p vo-dev -- test lint --suite lang --strict
cargo run -q -p vo-dev -- test coverage --suite lang --format json
cargo run -q -p vo-dev -- lint tasks
cargo run -q -p vo-dev -- lint tasks --strict
cargo run -q -p vo-dev -- task coverage --format json
cargo test -p vo-dev
```

Completion criteria:

- Strict lint can run before full migration by honoring an explicit allowlist.
- The allowlist is visible in repo policy and shrinks monotonically.
- New tests and tasks cannot be added without complete metadata.

## Work Package 2: Full Language Manifest Migration

Objective: migrate all 1068 language cases to complete metadata without losing
or silently changing jobs.

Migration method:

1. Generate a pre-migration catalog snapshot:

   ```sh
   cargo run -q -p vo-dev -- test catalog --suite lang --format json > before.json
   cargo run -q -p vo-dev -- test stats --suite lang --format json > before-stats.json
   ```

2. Migrate one domain batch at a time.
3. Run lint and catalog diff after each batch.
4. Preserve case ids.
5. Preserve resolved jobs unless the change intentionally fixes bad policy.
6. Record intentional job changes in the commit message and doc notes.

Batch order:

1. Compile-fail cases.
   - `matrix = "compile"`
   - `tags = ["typechecker", "compile-fail"]` or more precise compiler tags.
   - owner usually `typechecker`, `parser`, or `codegen`.

2. GC cases.
   - `matrix = "gc"`
   - `tags` include `gc` plus object domain, such as `slice`, `map`,
     `closure`, `interface`, `defer`, `scheduler`, or `barrier`.
   - owner `runtime-gc`.

3. JIT and OSR cases.
   - `matrix = "default"` when WASM/nostd are valid.
   - `matrix = "native"` when only VM/JIT/OSR are meaningful.
   - `tags` include `jit` or `osr`, plus `call`, `defer`, `interface`,
     `panic`, `metadata`, or `regression`.
   - owner `jit`.

4. Typechecker pass cases.
   - `tags` include `typechecker` and feature domain.
   - owner `typechecker`.

5. Module/project/zip cases.
   - `kind = "project"` or `kind = "zip"` remains physical input kind.
   - `tags` include `module`, `workspace`, `archive`, or `resolver`.
   - owner `module`.

6. Dyn/interface cases.
   - `tags` include `dyn` or `interface`.
   - owner `runtime` or `typechecker` depending on what is protected.

7. Stdlib and platform cases.
   - `tags` include `stdlib`, package name, and platform domain.
   - owner `stdlib` or `web-wasm`.

8. Scheduler/channel/island cases.
   - `tags` include `scheduler`, `channel`, `island`, `port`, or `async`.
   - owner `runtime`.

9. Core language/runtime cases.
   - `tags` include core feature names: `slice`, `map`, `struct`, `method`,
     `interface`, `panic`, `defer`, `numeric`, `control-flow`.
   - owner by domain.

10. Bug regression cases.
    - Keep `regression`.
    - Add the real domain tag; `bug` alone is not enough.
    - Owner must be the subsystem that would fix the bug.

Directory movement:

- Do not move files until metadata is complete.
- After migration, move cases into domain directories in small batches.
- Use catalog diff to prove no resolved jobs were lost.
- Preserve case ids even when paths move.

Acceptance gates:

```sh
cargo run -q -p vo-dev -- test lint --suite lang --strict
cargo run -q -p vo-dev -- test coverage --suite lang
cargo run -q -p vo-dev -- test stats --suite lang
cargo run -q -p vo-dev -- test catalog --suite lang --format json
./d.py test both
./d.py test osr
./d.py test gc
./d.py test wasm
```

Completion criteria:

- 0 cases missing matrix.
- 0 cases missing tags.
- 0 cases missing owner.
- 0 explicit target arrays without reason.
- 0 compile-fail cases without `matrix = "compile"`.
- 0 GC-looking cases selected only by filename.
- Catalog diff confirms intended job counts.

## Work Package 3: Task Graph Completion

Objective: make the task graph fully explainable and impossible to hide policy
inside task names or command strings.

Implementation work:

- Add group metadata while preserving current group arrays during migration.
- Require owner and tags for all tasks.
- Add `surface` or equivalent tag for each task:
  - `manifest-lint`
  - `lang-case`
  - `crate-unit`
  - `crate-integration`
  - `contract`
  - `model`
  - `docs-policy`
  - `example-smoke`
  - `benchmark`
  - `app-build`
  - `app-smoke`
  - `release-verify`
  - `legacy-excluded`
- Split current broad `test` group into named groups:
  - `lang-main`
  - `lang-backends`
  - `compile-contract`
  - `gc-contract`
  - `jit-contract`
  - `runtime-contract`
  - `typechecker-contract`
  - `codegen-contract`
  - `module-contract`
- Keep top-level `test`, `contract`, `site`, `release-verify`, `quality`,
  `full`, and `pr` as composed groups.
- Add task stats in JSON with group metadata.
- Add lint that fails if public tasks are not reachable from a top-level group.

Required owner mapping:

- `eng`: engineering lints, CI, tooling, d.py compatibility.
- `docs`: docs lint/sync.
- `runtime-gc`: GC language and crate contracts.
- `jit`: JIT, OSR, verifier, metadata, callback and materialization tests.
- `runtime`: scheduler, panic/defer, interface, dyn, pack, channel, island.
- `typechecker`: checker diagnostics and semantic type rules.
- `codegen`: bytecode generation and metadata layout contracts.
- `module`: module resolver, workspaces, zip/project cases.
- `stdlib`: standard library facade and native extern coverage.
- `web-wasm`: WASM runner, vo-web, browser/runtime compatibility.
- `studio`: Studio and quickplay local validation.
- `release`: release package policy.

Acceptance gates:

```sh
cargo run -q -p vo-dev -- lint tasks --strict
cargo run -q -p vo-dev -- task stats --format json
cargo run -q -p vo-dev -- task plan pr --format json
cargo run -q -p vo-dev -- task plan full --format json
cargo test -p vo-dev
```

Completion criteria:

- 0 tasks missing owner.
- 0 tasks missing tags.
- 0 top-level groups missing metadata.
- 0 public tasks unreachable from top-level groups.
- `contract` group includes GC/JIT/runtime/typechecker/codegen/module
  contract coverage.
- `stress` group exists and contains only explicitly heavier checks.

## Work Package 4: Unified Native And WASM Results

Objective: make language results machine-readable and identical across native
and WASM.

Implementation work:

- Update `lang/crates/vo-web/test_runner.mjs` to emit `volang.test-result.v1`.
- Carry plan metadata through WASM results:
  - matrix
  - tags
  - owner
  - expect
  - skip/failure reason
- Normalize WASM compile diagnostics into the same fields native uses.
- Let `vo-dev test run --format json` support mixed native/WASM selections.
- Aggregate native and WASM outputs in `vo-dev` rather than printing separate
  result documents.
- Add schema tests for:
  - native-only run
  - wasm-only run
  - mixed default matrix run
  - compile-fail run
  - GC run
  - skipped target
  - expected failure
  - differential mismatch

Acceptance gates:

```sh
cargo run -q -p vo-dev -- test run --suite lang --matrix default --path tests/lang/cases/hello.vo --format json
cargo run -q -p vo-dev -- test run --suite lang --targets wasm --path tests/lang/cases/hello.vo --format json
cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit,wasm --path tests/lang/cases/hello.vo --format json
cargo test -p vo-dev
cargo test -p vo-test
```

Completion criteria:

- Native and WASM results validate against one schema.
- Mixed native/WASM JSON returns one document.
- Result jobs contain matrix, tags, owner, expectation, status, and duration.
- Differential baselines are represented in result metadata.

## Work Package 5: Explainable Selection

Objective: any developer can ask why a task, case, or target was selected and
get a concrete answer.

Implementation work:

- Add `vo-dev task plan ... --explain`.
- Add `vo-dev test plan ... --explain`.
- Add `vo-dev test explain --suite lang --case <id>`.
- Add JSON and text explain output.
- Track task reasons:
  - selector group
  - task input match
  - known prefix rule
  - dependency expansion
  - downstream dependent expansion
  - fallback for unknown path
  - explicit CLI selector
- Track case reasons:
  - path filter
  - matrix filter
  - tag filter
  - owner filter
  - default matrix expansion
  - explicit target override
  - skip reason
  - expected-fail reason
- Add CI matrix metadata with selected task reasons.
- Add lint that reports unknown-path fallback counts.

Acceptance gates:

```sh
cargo run -q -p vo-dev -- task plan pr --changed --explain
cargo run -q -p vo-dev -- task plan pr --changed --format json --explain
cargo run -q -p vo-dev -- test plan --suite lang --tags gc --explain
cargo run -q -p vo-dev -- test explain --suite lang --case gc-closure-capture
```

Completion criteria:

- Every selected task has at least one non-empty reason.
- Every selected test job has a case-selection reason and target-selection
  reason.
- Unknown path fallback is visible and rare.
- Known high-risk paths select named contract groups without relying on broad
  fallback.

## Work Package 6: Contract Coverage Completion

Objective: make high-risk cross-layer behavior first-class and owned.

Required contract groups:

- `gc-contract`
- `jit-contract`
- `runtime-contract`
- `typechecker-contract`
- `codegen-contract`
- `module-contract`
- `release-contract`
- `docs-contract`
- `app-contract`

GC contract must cover:

- allocator behavior
- mark/sweep state transitions
- root table behavior
- precise stack roots
- interface slots
- closure captures
- defer/panic paths
- scheduler boundaries
- JIT spills and materialization
- VM-call materialization
- write barriers
- layout validation
- model tests for object graph liveness

JIT contract must cover:

- semantic fact rows
- verifier requirements
- metadata schema and fail-fast behavior
- runtime helper ABI
- callback ABI
- direct call materialization
- side-exit classification
- OSR state transfer
- VM/JIT differential behavior
- GC may-call boundaries

Runtime contract must cover:

- panic/defer unwinding
- scheduler blocking and wakeup semantics
- channel and port queues
- dynamic values
- interface representation
- slice/map/string invariants
- stdlib/native extern ABI where runtime-owned

Typechecker/codegen/module contract must cover:

- compile-fail diagnostics
- method sets
- assignability
- metadata emitted for runtime and JIT
- bytecode serialization assumptions
- project and zip inputs
- workspace/module resolution
- native/WASM compatibility boundaries

Acceptance gates:

```sh
cargo run -q -p vo-dev -- task plan contract --format json
cargo run -q -p vo-dev -- task run contract
cargo run -q -p vo-dev -- test run --suite lang --tags contract --format json
cargo run -q -p vo-dev -- test run --suite lang --tags gc --format json
cargo run -q -p vo-dev -- test run --suite lang --tags jit --format json
```

Completion criteria:

- Each high-risk domain has a contract group.
- Each contract group contains at least one crate-level task and, where
  applicable, language-level coverage.
- Contract groups are selected by smart CI for relevant source paths.
- `contract` is no longer just GC.

## Work Package 7: Stress, Model, And Repeat Coverage

Objective: cover bugs that normal deterministic regression tests do not expose
without making PR checks unusably slow.

Implementation work:

- Add `stress` tier tasks for:
  - GC stress repeats
  - JIT/OSR differential repeats
  - scheduler/channel stress
  - parser/typechecker fuzz-like corpus smoke
  - module resolver edge corpus
- Add model tests for:
  - GC root graph liveness
  - scheduler queues
  - bytecode metadata invariants
  - typechecker method-set rules where compact models are useful
- Add bounded runtime budgets to stress tasks.
- Add repeat-count configuration in `eng/tests.toml` or task env, not shell
  loops hidden in commands.
- Add `nightly` or `stress` group separate from normal PR validation.

Acceptance gates:

```sh
cargo run -q -p vo-dev -- task plan stress --format json
cargo run -q -p vo-dev -- task run stress
cargo run -q -p vo-dev -- task stats
```

Completion criteria:

- Stress tasks are discoverable and owned.
- Stress tasks are not hidden in normal `test` unless cheap enough.
- Every stress task has a timeout budget and owner.
- New flaky stress failures can be quarantined only with reason and owner.

## Work Package 8: App, Docs, Release, And Legacy Surfaces

Objective: make non-language validation part of the same visible system.

Implementation work:

- Add metadata for docs, examples, benchmarks, app/site, release, and legacy
  tasks.
- Add catalog/stats for examples and benchmarks manifests.
- Add docs policy result shape to task output where useful.
- Add explicit `legacy-excluded` inventory for `vo-playground`.
- Decide final fate of playground legacy:
  - keep as explicit legacy surface,
  - restore into app-site validation,
  - or remove from workspace.
- Add Studio/quickplay smoke ownership and site grouping.
- Ensure release verify tasks have `owner = "release"` and release tags.

Acceptance gates:

```sh
cargo run -q -p vo-dev -- lint examples
cargo run -q -p vo-dev -- lint benchmarks
cargo run -q -p vo-dev -- lint docs
cargo run -q -p vo-dev -- lint release
cargo run -q -p vo-dev -- task plan site --format json
cargo run -q -p vo-dev -- task plan release-verify --format json
```

Completion criteria:

- Non-language surfaces appear in task stats by owner/tag.
- Legacy excluded surfaces are explicit.
- Site and release checks are not hidden inside generic test groups.

## Work Package 9: Documentation And Developer Workflow

Objective: make the finished system understandable without reading all of
`cmd/vo-dev`.

Required docs:

- `lang/docs/dev/test-system-target-state.md`: architecture and target state.
- `lang/docs/dev/test-system-completion-plan.md`: this execution plan.
- `lang/docs/dev/test-authoring-guide.md`: how to add a case, contract task,
  skip, expected failure, or stress test.
- `lang/docs/dev/test-ci-routing-guide.md`: how smart CI selects work and how
  to debug selection.
- Short references in `skills/volang-dev` only where needed; avoid duplicating
  policy.

The authoring guide must include:

- language case examples
- compile-fail examples
- GC/JIT contract examples
- explicit target override examples
- skip and timeout examples
- owner/tag naming rules
- commands to run before opening a PR

Acceptance gates:

```sh
node scripts/ci/docs_lint.mjs
cargo run -q -p vo-dev -- lint docs
```

Completion criteria:

- A new contributor can add a test by following one guide.
- Docs do not duplicate executable policy.
- Docs lint proves generated mirrors are synchronized.

## Work Package 10: Final Validation And Lock-In

Objective: prove the system is complete and prevent regression.

Final validation sequence:

```sh
cargo fmt --all -- --check
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- lint tasks --strict
cargo run -q -p vo-dev -- test lint --suite lang --strict
cargo run -q -p vo-dev -- test coverage --suite lang
cargo run -q -p vo-dev -- task coverage
cargo run -q -p vo-dev -- test catalog --suite lang --format json
cargo run -q -p vo-dev -- test stats --suite lang --format json
cargo run -q -p vo-dev -- task stats --format json
cargo run -q -p vo-dev -- task plan pr --changed --explain
cargo run -q -p vo-dev -- task plan full --format json
cargo test -p vo-dev
cargo test -p vo-test
cargo check --workspace --all-targets --exclude vo-playground
./d.py test both
./d.py test osr
./d.py test gc
./d.py test wasm
cargo run -q -p vo-dev -- task run contract
cargo run -q -p vo-dev -- task run site
cargo run -q -p vo-dev -- task run release-verify
```

Final lock-in rules:

- Remove migration allowlist entries.
- Enable strict lints in `quality` or `pr`.
- Make metadata coverage regressions fail CI.
- Make result schema regressions fail tests.
- Make unknown CI fallback visible in PR output.
- Keep `d.py` as a facade only.

Completion criteria:

- 0 missing case metadata.
- 0 missing task metadata.
- 0 missing group metadata.
- 0 unowned skips, failures, timeouts, or target overrides.
- 0 mixed-result schema gaps.
- 0 broad CI fallbacks for known repo paths.
- 0 hidden test policy outside `eng/*.toml`, `cmd/vo-dev`, manifests, or
  runner implementations.

## Work Order

Recommended order:

1. Schema and strict lint infrastructure.
2. JSON stats and coverage commands.
3. Full language manifest metadata migration.
4. Task/group metadata completion.
5. WASM/native result schema unification.
6. Explainable task and case selection.
7. Contract groups beyond GC.
8. Stress/model layer.
9. App/docs/release/legacy catalog completion.
10. Directory cleanup.
11. Final strict-mode lock-in.

Do not start directory movement before manifest metadata coverage reaches 100%.
Do not enable strict CI gates before the migration allowlist is empty. Do not
add new test runners unless the existing runner boundary cannot express the
needed result schema.

## What Should Not Remain After Completion

These are signs the plan is not done:

- A case can be added with only `targets`.
- A case can skip a target without a reason.
- A GC case is selected because its filename contains `gc`.
- A task appears in stats as owner `(missing)`.
- A task group is just a name and an array.
- Native and WASM JSON results have different shapes.
- CI says only that a task was selected, not why.
- A high-risk source path falls back to the broad PR set because no precise
  rule exists.
- `contract` means only GC.
- Directory names are used as implicit selection policy.
- A developer must read shell scripts or workflow YAML to understand test
  policy.

If any of these remain, the system is improved but not complete.
