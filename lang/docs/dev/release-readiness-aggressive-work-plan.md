# Aggressive Architecture And Release Readiness Work Plan

Status, 2026-06-30: this is the current execution plan for taking `main` from
the merged hardening/refactor state to a release-ready state while also pushing
down the architectural debt that would otherwise keep producing the same
release blockers.

This supersedes the release-only version of this plan. It still starts after
the industrial uncommitted-worktree closure described in
`lang/docs/dev/uncommitted-industrial-github-ci-plan.md` and
`lang/docs/dev/uncommitted-hardening-refactor-plan.md`, but it is no longer
limited to final gate refresh. The working assumption is: when a release blocker
exposes a weak architecture boundary, fix the boundary now if it can be tested,
reviewed, and bisected cleanly.

This document is not completion proof. Completion is determined only by current
source, `eng/*.toml`, `cmd/vo-dev`, `tests/lang/manifest.toml`, generated
evidence produced through declared tasks, local gate output, GitHub PR state,
and required CI checks.

## Goal

Make the current repository state release-grade and structurally harder to
break again:

1. Fix the active P0 blockers.
2. Replace release-time "be careful" rules with source-enforced or
   task-enforced checks.
3. Collapse duplicate policy into `eng/*.toml` plus `cmd/vo-dev`.
4. Separate current product surfaces from legacy surfaces instead of mixing
   their risk profiles.
5. Verify browser, module-release, VM/JIT, evidence, and docs paths through the
   task graph.
6. Publish the result through the smallest practical GitHub surface: normally
   one `codex/` branch, one draft PR, and CI follow-up only when the local gate
   is already coherent.

## Non-Goals

- Do not add user-facing language features.
- Do not delete regression tests or generated artifacts to reduce review size.
- Do not hand-edit generated docs, quickplay artifacts, or gate evidence.
- Do not put new policy in `d.py`, workflow YAML, or docs JS scripts.
- Do not trust dirty local sibling repos as release/stage proof.
- Do not create extra GitHub branches, PRs, comparison links, or rerun loops
  unless they directly move the active PR toward green.
- Do not perform real release publish, package upload, or Homebrew tap mutation
  as part of verification.
- Do not do speculative hot-path rewrites without a concrete guard, test, or
  reviewable module boundary.

## What Aggressive Means Here

This plan is aggressive in these concrete ways:

1. P0 work includes architecture repair, not only symptom fixes.
2. Browser startup must be proven by a real smoke path, not only by a Vite
   build.
3. VM production evidence drift becomes a failing `vo-dev` lint, not a note in
   a document.
4. First-party release verification uses clean roots or fails fast; dirty
   sibling worktrees are not a valid input.
5. Current web dependency audit becomes a task-graph gate, while legacy audit
   debt is either fixed or explicitly excluded by data policy.
6. Final gate selectors, readiness evidence rules, release repo roots, artifact
   policy, and docs mirror policy move toward one data/control plane.
7. Source-contract compact helpers are stabilized and duplicate local scanners
   are removed where that can be done mechanically.
8. Misleading JIT "fallback" wording is corrected, and future wording drift
   should be lintable where practical.
9. Large structural moves are allowed when they are pure movement, covered by
   focused tests, and kept separate from behavior changes.

## Current Baseline

Planning baseline:

- Volang worktree: clean on `main` before this plan document was added.
- Source state: `eb4a1fa20b5a5941430250515bab75f50d56d1ea`.
- Previous hardening closure PRs have already merged; the old giant
  uncommitted worktree plan is historical context, not the active defect list.
- `cargo run -q -p vo-dev -- task plan pr` and
  `cargo run -q -p vo-dev -- verify plan pr` were ready in the current clean
  tree during planning.
- Studio and `vo-web` npm audit were clean during planning, while
  `apps/playground-legacy` still had audit findings and is marked legacy in
  toolchain data.

Active release blockers:

1. Studio was upgraded to Svelte 5, but `apps/studio/src/main.ts` still uses the
   old component class construction shape.
2. `lang/docs/dev/vm-production-readiness.md` still says production signoff is
   withdrawn, and checked-in final-gate evidence is not a trustworthy current
   source-state proof.
3. First-party release/stage verification can accidentally read dirty or stale
   sibling working trees, especially local `vogui` and `voplay`.

Active architecture debts to push down in this same effort:

1. `vo-dev` does not yet reject stale/mismatched VM production evidence.
2. Release/stage logic does not sufficiently distinguish clean hermetic inputs
   from local workspace state.
3. Current and legacy Node workspaces are not separated by a visible audit
   policy gate.
4. Studio browser execution has weaker automation than its build task.
5. Some task/evidence/release selectors still exist as duplicated Rust lists
   instead of data-owned policy.
6. Source-contract scanner helpers are still duplicated in some test modules.
7. Studio/JIT docs can drift toward broad fallback wording that does not match
   strict JIT runtime-path policy.

## Execution Strategy

Use three local work waves if the diff grows too large, but do not defer a P0
architecture guard merely because it is not a one-line fix. These waves are
planning boundaries, not a request to create multiple GitHub PRs. Default to one
draft PR unless review size or CI isolation makes a split clearly better.

Wave 1: stop user-visible and release-visible breakage.

- Studio Svelte 5 mount migration.
- Browser smoke task.
- Current Node workspace audit gate.
- Legacy/current web policy split.

Wave 2: make release evidence and staging hermetic.

- VM production readiness/evidence lint.
- Data-owned final selector/evidence requirements.
- Clean first-party release roots.
- Dirty/stale sibling fail-fast.

Wave 3: flatten structural debt that keeps review and CI brittle.

- Source-contract helper consolidation.
- Docs/JIT runtime-path wording guard.
- Remaining small policy de-duplication.
- Optional pure-move runtime/JIT test module splits if the touched tests are
  too large to review.

If a wave becomes too big, split commits first. Split PRs only when the current
single PR has become genuinely hard to review or impossible to validate as one
unit. Do not refresh final evidence until the source shape is final.

## P0 Track A. Studio Runtime And Browser Proof

The current Studio bundle can build while still failing at browser startup. The
entrypoint should use Svelte 5 `mount(App, { target })` instead of casting the
component to a class and calling `new`.

Required work:

1. Migrate `apps/studio/src/main.ts` to the Svelte 5 mount API.
2. Avoid Svelte class-compat shims unless source inspection proves they are
   necessary for another entrypoint.
3. Add or wire a browser smoke task that boots the built Studio app, checks that
   the main UI is mounted, verifies WASM preload is not crashing, and reaches a
   quickplay visible first frame when feasible.
4. Keep browser smoke in `eng/*.toml` plus `cmd/vo-dev`; do not hide it as a
   loose script step.
5. Ensure CI can run the smoke without manual browser interaction.

Focused validation:

```sh
./d.py ci task studio-build
./d.py ci task studio-wasm-build
./d.py ci task quickplay-validate
./d.py ci task blockkart-smoke-static
./d.py ci site
```

Additional validation after the smoke task exists:

```sh
cargo run -q -p vo-dev -- task run task:studio-browser-smoke
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- verify plan pr
```

Architecture exit criteria:

- Studio no longer depends on the Svelte 4 component class API.
- Browser boot is a repeatable gate, not a manual note.
- Quickplay artifact validation and browser runtime validation remain distinct.

## P0 Track B. Web Dependency Policy Split

Current Studio and `vo-web` dependency hygiene should be guarded continuously.
Legacy Playground should not silently poison or silently disappear from release
policy.

Required work:

1. Add a data-driven Node audit task for current non-legacy workspaces.
2. Make `apps/playground-legacy` an explicit legacy audit exemption in data, or
   fix its audit findings in a separate commit if the current release surface
   still claims it.
3. Keep npm audit policy in `eng/*.toml` and `vo-dev`, not a workflow YAML
   one-off.
4. Make the release/site plan show which Node workspaces are current, legacy,
   or excluded.

Focused validation:

```sh
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- verify plan pr
```

Run audit through the task graph once available. Manual workspace-level audit
commands are acceptable only as focused debugging while building the gate.

Architecture exit criteria:

- Current web audit can fail CI.
- Legacy audit debt is visible and data-owned.
- There is no implicit "all package-locks are release blockers" ambiguity.

## P0 Track C. VM Production Evidence Guard

The readiness document cannot be used as release evidence while it says signoff
is withdrawn and the evidence files do not prove the current source state.
There is also a guard gap: docs lint is thin and currently does not reject
stale or mismatched VM production evidence.

Required work:

1. Add a `vo-dev`-owned readiness/evidence lint.
2. Check evidence schema, required final selectors, source-state consistency,
   task status, and readiness-document claims.
3. Make the lint fail on the current drift before refreshing evidence.
4. Wire the lint into the existing lint surface without adding policy to
   `scripts/ci/docs_lint.mjs`.
5. Move any hard-coded final selector list toward task metadata or an
   `eng/*.toml` data source.
6. Run final selectors from the final source state and update evidence only
   through declared tasks.
7. Update `vm-production-readiness.md` only to match generated and validated
   evidence.

Focused validation:

```sh
cargo test -p vo-dev lint_system
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- lint tasks --strict
cargo run -q -p vo-dev -- task final-selectors --format json
cargo run -q -p vo-dev -- task run contract
cargo run -q -p vo-dev -- task run vm-production
cargo run -q -p vo-dev -- task run site
cargo run -q -p vo-dev -- task run release-verify
```

Architecture exit criteria:

- Evidence drift is impossible to miss in normal lint.
- Final selectors are not maintained as an unreviewed second list.
- Readiness prose cannot claim a source state that JSON evidence does not
  prove.

## P0 Track D. Hermetic First-Party Release Roots

Release verification must not trust local sibling directories when those trees
are dirty, behind upstream, or selected as local members by `vo.work`. The staging path
must either use clean checkouts or fail fast with a clear preflight error.

Required work:

1. Add release/stage preflight that rejects dirty first-party roots and stale
   upstream state when the command intends to use local source.
2. Prefer clean checkout roots or explicit CI module roots for first-party
   release verification.
3. Ensure local `vo.work` member/source selection cannot accidentally make release verification
   differ from release staging.
4. Add tests for dirty tree, ahead/behind, missing upstream, and clean checkout
   cases where practical.
5. Keep current local sibling user changes untouched.
6. Run first-party release verification from isolated clean roots.

Focused validation:

```sh
cargo test -p vo-release
cargo test -p vo-module
cargo run -q -p vo-dev -- lint release
./d.py ci release-verify
./d.py ci task release-verify-vogui
./d.py ci task release-verify-voplay
./d.py ci task release-verify-vopack
./d.py ci task release-verify-vostore
```

Architecture exit criteria:

- Dirty sibling repos are not valid release proof.
- CI and local release verification use the same source-root semantics.
- Release/stage errors point to the exact repo and cleanliness problem.

## P1 Track E. Engineering Policy Data Plane

Policy duplication is a root cause, not a cosmetic issue. The target shape is:
`eng/*.toml` owns data, `cmd/vo-dev` interprets it, workflows and JS scripts
consume `vo-dev` outputs.

Required work:

1. Inventory hard-coded selector and release-policy lists in `cmd/vo-dev`.
2. Move final selectors, readiness evidence requirements, current/legacy Node
   workspace audit status, docs mirror metadata, and first-party repo policy
   into data where the migration is straightforward.
3. Keep workflow YAML as execution wrappers over `vo-dev` matrix/metadata.
4. Keep `scripts/ci/docs_lint.mjs` and `scripts/ci/docs_sync.mjs` as narrow
   docs/generation utilities, not broad policy engines.
5. Add lint coverage that rejects future policy duplication when practical.

Focused validation:

```sh
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- lint tasks --strict
cargo run -q -p vo-dev -- task plan pr --changed
cargo run -q -p vo-dev -- ci matrix pr --base <base> --head <head>
cargo run -q -p vo-dev -- verify plan pr
```

Architecture exit criteria:

- The PR description can point reviewers to the data source for each policy.
- No new task, release, audit, evidence, or docs policy is introduced only in
  YAML, `d.py`, or JS.
- Any remaining duplicated list has a named follow-up and a reason it was not
  moved in this wave.

## P1 Track F. Source-Contract And Test Architecture Cleanup

Source-contract scanner tests are valuable, but duplicated compact-source helper
logic makes them hard to review and easy to drift.

Required work:

1. Provide stable test-facing compact helper APIs from the source-contract
   support crate.
2. Replace duplicated local compact scanner helpers where this is mechanical.
3. Remove version-suffixed helper usage from new tests.
4. Preserve local red-team probes for each dangerous subsystem.
5. Split oversized touched tests into modules when doing so is pure movement.

Focused validation:

```sh
rg 'compact_.*_0[0-9]+' lang/crates cmd tests
cargo test -p vo-source-contract
cargo test -p vo-vm --features jit source_contract
cargo test -p vo-jit source_contract
cargo test -p vo-dev lint_system
```

Architecture exit criteria:

- Test helpers have one stable owner.
- Subsystem tests keep their specific assertions.
- Mechanical test moves are not mixed with semantic fixes.

## P1 Track G. JIT Runtime-Path Wording And Guard

Strict JIT errors, side exits, and VM call materialization are distinct
contracts. Broad "fallback" wording makes user docs and maintainer intuition
worse.

Required work:

1. Update Studio docs and developer docs that describe intentional JIT runtime
   paths.
2. Prefer "side exit", "VM call materialization", "runtime helper", or explicit
   unsupported-state wording.
3. Consider a narrow docs lint for high-risk public docs if the wording keeps
   drifting.
4. Do not rename manifest side-exit observation fields unless test-system
   ownership requires it.

Focused validation:

```sh
./d.py ci task docs-lint
cargo run -q -p vo-dev -- lint all
cargo test -p vo-jit
cargo test -p vo-vm --features jit
```

Architecture exit criteria:

- Public docs no longer imply strict JIT silently falls back.
- Runtime-path terminology matches `vo-jit` and `vo-vm` contracts.

## P2 Track H. Runtime/JIT Module Boundary Pushdown

Do not rewrite hot paths for style, but do take pure structural wins that make
future correctness work easier.

Allowed work:

1. Split newly touched large VM/JIT tests into ownership modules.
2. Extract obviously coherent cold-path helper modules from
   `runtime_boundary.rs` only when the extraction is mechanical and covered by
   VM/JIT boundary tests.
3. Collapse stale comments around JIT semantic rows, metadata contracts, and
   runtime path policy.
4. Remove parallel opcode-family facts only when the semantic row source can
   express the same contract and tests prove parity.

Not allowed in this wave:

- A behavior-changing rewrite of scheduler execution.
- A new JIT lowering architecture without a failing test or row-source
  migration need.
- GC root strategy changes without dedicated GC contract coverage.

Focused validation:

```sh
cargo test -p vo-vm runtime_boundary
cargo test -p vo-vm --features jit
cargo test -p vo-jit
./d.py test jit
./d.py test osr
./d.py test gc
cargo run -q -p vo-dev -- task run gc-contract
```

Architecture exit criteria:

- Any structural movement makes future ownership clearer.
- Behavior changes, if any, have their own commit and regression test.
- Hot-path performance-sensitive changes are either avoided or separately
  justified.

## Commit Map

Suggested commit sequence:

1. `docs: record aggressive architecture readiness plan`
   - Adds this plan.
   - Validation: Markdown-only review, `git diff --check`, docs lint.
2. `web: migrate Studio entrypoint to Svelte 5 mount`
   - Fixes the browser startup blocker.
   - Validation: Studio build and focused app tasks.
3. `web: add Studio browser smoke gate`
   - Turns browser boot into a repeatable task.
   - Validation: browser smoke, site task.
4. `eng: audit current Node workspaces`
   - Adds data-owned audit coverage and legacy/current separation.
   - Validation: audit task and `vo-dev lint all`.
5. `eng: lint VM production readiness evidence`
   - Adds a failing guard for stale/mismatched final evidence.
   - Validation: `cargo test -p vo-dev`, `vo-dev lint all`.
6. `eng: move final gate evidence policy into data`
   - Reduces selector/policy duplication.
   - Validation: task final-selectors, task plan, lint tasks.
7. `release: require clean first-party release roots`
   - Makes release/stage hermetic or fail-fast.
   - Validation: `cargo test -p vo-release`, release verify tasks.
8. `release: verify first-party modules from isolated roots`
   - Applies the clean-root model to current first-party checks.
   - Validation: first-party release verify tasks.
9. `test: consolidate source-contract compact helpers`
   - Mechanical helper cleanup.
   - Validation: source-contract and affected tests.
10. `docs: align JIT runtime-path wording`
    - Corrects public/developer terminology.
    - Validation: docs lint and JIT focused tests if source comments move.
11. `vm/jit: split touched boundary tests by ownership`
    - Optional pure movement if test files touched by earlier work remain too
      large.
    - Validation: VM/JIT focused tests.
12. `docs/evidence: refresh VM production gate evidence`
    - Generated evidence and readiness update for final source state.
    - Validation: final selectors, `vo-dev lint all`, final gate set.

If a commit grows beyond one reason, split it. Behavior fixes, mechanical moves,
policy migration, generated evidence, and wording cleanup must stay separate.

## Final Local Gate

Before opening or updating the final PR, run the broad gate set unless a command
is impossible in the current environment and the reason is documented.

```sh
cargo fmt --all -- --check
git diff --check
cargo check --workspace --all-targets --exclude vo-playground
cargo run -q -p vo-dev -- lint all
cargo run -q -p vo-dev -- test lint --suite lang --strict
./d.py test both
./d.py test jit
./d.py test osr
./d.py test nostd
./d.py test wasm
./d.py test gc
cargo run -q -p vo-dev -- task run contract
cargo run -q -p vo-dev -- task run site
cargo run -q -p vo-dev -- task run release-verify
cargo run -q -p vo-dev -- task run vm-production
```

Add focused checks when a touched subsystem requires them:

```sh
cargo test -p vo-jit
cargo test -p vo-vm --features jit
cargo test -p vo-release
cargo test -p vo-module
cargo test -p vo-runtime --release resolved_call_rejects_provider_identity_drift_after_load
cargo check -p vo-ext --target wasm32-unknown-unknown
```

Do not run real release publish commands as verification.

## GitHub Plan

GitHub is the merge and CI surface, not the main workspace. Keep GitHub
operations quiet and purposeful.

Rules:

1. Use one `codex/` branch for this effort unless the user explicitly asks for
   a split.
2. Open one draft PR after local focused gates pass.
3. Push in meaningful batches after commits are coherent; do not push every
   experiment.
4. Do not create comparison-only branches, exploratory PRs, or side PRs just to
   expose intermediate state.
5. Do not ask the user to operate GitHub manually.
6. Use `gh`/GitHub only for:
   - creating or updating the active draft PR,
   - reading required check status,
   - reading logs for failing jobs,
   - rerunning jobs only after source/config has been fixed or the failure is
     clearly external.

Draft PR contents:

1. Active blocker summary.
2. Architecture debt pushed down.
3. Commit map.
4. Local validation table.
5. Generated evidence provenance.
6. Sibling checkout/isolation note.
7. Reviewer guide.

Execution:

1. Create the `codex/` branch after the first coherent commit.
2. Push after local focused gates pass.
3. Open the draft PR with:
   - active blocker summary,
   - architecture debt pushed down,
   - commit map,
   - local validation table,
   - generated evidence provenance,
   - sibling checkout/isolation note,
   - reviewer guide.
4. Follow required GitHub checks until green.
5. For CI failures:
   - read the failing job log,
   - identify the exact task,
   - reproduce locally when possible,
   - fix source/config,
   - push a new commit.

CI reruns are only for likely external flakes. Do not bypass a failing policy,
remove coverage, or create extra GitHub objects to make CI look quieter.

## Reviewer Guide

Review in this order:

1. Studio mount migration and browser smoke evidence.
2. Current/legacy Node audit policy split.
3. Readiness/evidence lint logic and failure messages.
4. Data ownership for final selectors and evidence policy.
5. Release/stage clean-root preflight behavior.
6. First-party clean checkout verification.
7. Source-contract helper consolidation.
8. JIT/runtime-path wording cleanup.
9. Generated evidence provenance and source-state consistency.

The important question is not "did the old huge plan mention this risk?" but
"does the current source plus gate output make this class of failure hard to
repeat?"

## Remaining Risks

- `apps/playground-legacy` audit findings are not a current Studio release
  blocker only if legacy status remains explicit in policy.
- Local `vogui`, `voplay`, and Homebrew tap directories may remain dirty or
  behind; they must not be used as release proof unless isolated or cleaned.
- BlockKart source regeneration may be blocked when the external source tree is
  unavailable; checked-in quickplay artifacts can still be statically
  validated.
- Native/Tauri Studio does not yet have a dedicated task-graph build gate. If
  desktop Studio is in release scope, add it as another current app task.
- Large VM/JIT source-contract scanner tests remain valuable but brittle; this
  plan permits mechanical helper consolidation and test splitting, not
  weakening probes.
- Deep runtime-boundary decomposition may exceed one PR if it stops being pure
  movement. In that case, land the guarded release work first and keep the
  decomposition PR next in line, not as an undefined someday task.

## Exit Criteria

The work is complete only when:

1. P0 blockers are fixed in source and covered by focused validation.
2. Browser Studio boot is a repeatable gate.
3. Current web dependency audit is guarded, and legacy audit scope is explicit.
4. VM production evidence is current for the final source state and guarded by
   lint.
5. Final selectors and readiness evidence policy have a data-owned source or a
   documented temporary duplication with a follow-up.
6. First-party release verification is isolated from dirty sibling worktrees or
   fails fast.
7. Source-contract helper duplication is reduced where touched, without
   weakening red-team tests.
8. Final local gates pass or any environment-only exceptions are documented.
9. A GitHub draft PR exists with the planned commit map.
10. Required GitHub checks are green.
11. The final handoff lists completed items, unfinished items, commit/PR split,
    local validation, PR URL, CI status, remaining risks, and reviewer guide.
