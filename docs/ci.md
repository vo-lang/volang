# Continuous integration and delivery

Volang uses one declared CI task model, source-bound evidence, and promotion of
tested bytes. The machine-readable sources of truth are:

- `eng/ci.toml` for pull-request, merge, main, and Nightly task profiles;
- `eng/tests.toml` and `tests/lang/manifest.toml` for language cases and costs;
- `eng/release.toml` for release targets and archive policy;
- `eng/toolchains.toml` and `rust-toolchain.toml` for pinned tools.

Workflow YAML provisions and schedules lanes while task execution migrates into `vo-dev ci run`. Repository contracts, Rust quality checks and dependency audits
execute ordered command definitions from `eng/ci.toml`. `vo-dev ci lint` validates the task graph,
dependencies, owners, platforms, budgets, runners, and safe evidence paths.
Actionlint validates the workflow syntax and expressions.

## Trust model

Each CI run follows the same chain:

1. `vo-dev ci plan` selects an immutable task set and records the source commit,
   Git tree, profile, changed paths, and complete task definitions. Impact plans
   also bind resolved base/head/merge-base object IDs and a component graph digest.
2. A migrated job calls `vo-dev ci run --plan <path> --task <id>`. Its receipt
   binds the task, source, CI plan, toolchain files, test manifests, runner,
   GitHub run/job identity, timing, result files, and promotable artifacts. Domain results must identify a complete test or browser
   scenario; arbitrary success flags are rejected. Unmigrated jobs temporarily
   use `ci record`; migrated tasks reject that entry point.
3. The stable `required` job rejects incomplete lanes and calls
   `vo-dev ci certify`. Certification requires one valid receipt for every
   planned task, with no missing, duplicate, or extra receipt.
4. Site and release workflows verify that bundle against their checkout. A
   promotable artifact is hashed recursively and must match the exact bytes
   recorded by its producing job.

Local processes can build plans for inspection. The official workflows record
receipts only inside GitHub Actions from a clean tracked worktree. Site and
release promotion accept bundles downloaded from the exact successful main CI
run, so a local or unrelated-workflow `passed: true` file has no deployment
authority.

Each executor attempt writes to a fresh `target/ci/executions/<task>/<attempt>`
directory. It archives earlier outputs, locks the task's local resource group,
captures command stdout/stderr, and compares declared source inputs before and
after execution. Task and command deadlines terminate the complete process
group or Windows Job Object. Cancellation uses the same cleanup path. Commands
run once; no automatic retry or shell interpolation is involved.

`started.json` explicitly marks an incomplete attempt. The executor atomically
writes the typed `result.json` and its digest in `completion.json`, then publishes
certifiable evidence as its final commit point. Certification errors are also
written to `failure.json` and cause a nonzero exit. Local dirty executions can
produce diagnostic receipts, but cannot produce certifiable evidence. Bundles
reject mixed GitHub run attempts and missing command or log records.

Rust test commands require a nonempty successful test set, independently of the
process exit code. Failure records identify the command, owner through the task
definition, classification, reproduction command and diagnostic paths. Job
summaries show per-command test counts and durations; Cargo HTML timings are
uploaded with executor diagnostics. Workspace tests continue across failing
test programs so one run reports all failures while preserving a failing exit.

## Explain and diagnose

`vo-dev ci explain --base <commit> --head <commit>` explains each task's inclusion
or exclusion. Deleted paths and both sides of a rename participate in selection.
The checked-out candidate must match `--head`. The planner joins component
contracts from both revisions and the candidate, walks reverse dependencies,
and maps affected capabilities to profile tasks. Rust edges come from workspace
Cargo manifests, including build, dev, optional and platform dependencies. Vo,
browser product inputs use the small component declarations in `eng/ci.toml`.
Generated source dependencies come from `eng/artifacts.toml`, including Studio's
embedded documentation. Explanations include each input, component chain and capability.
Shared controls, unknown inputs and missing historical graphs select the full
eligible profile. Manually supplied `--changed-file` plans remain local diagnostics
and cannot certify a candidate. The historical 32-case coverage
migration is recorded in `eng/ci-coverage.json`; external Voplay commands require
an explicitly provisioned, clean `eng/project.toml` pin. Retired Vogui commands
refer to a historical implementation, and active UI coverage belongs to the
renderer-neutral workspace and the platform/browser matrix.

A language runner's exit status, result schema, case/backend identities and
individual outcomes must agree with the selected plan. Unexpected, missing,
duplicate, skipped or unidentifiable jobs cannot certify success. Nonzero exits
cannot be hidden by a successful JSON payload.

Native filesystem ordering always runs. The symlink regression independently
probes the host using Rust before invoking Vo. Supported hosts exercise relative
file and directory links, absolute targets, dangling links and ReadDir/Lstat
metadata. Unavailable hosts must return an error and leave no entry. Set
`VO_TEST_REQUIRE_SYMLINK=1` in environments that require this capability.

Animation contracts use an explicitly installed per-VM manual clock shared by
stdlib time reads and timer completions. They test intermediate values,
cancellation, completion and actual JIT execution without sleeping.

Module publication (`vo-release`) requires anchored, durable directory
publication, currently implemented on Linux and macOS. Its publication journeys
declare `cfg(unix)`; Windows runs the portable source and artifact validation
tests and an explicit unsupported-host test that requires
`AtomicPublishUnsupported` and no filesystem output. Validation precedes the
host publication boundary. Windows distribution archives, CLI execution,
Native AOT and desktop UI remain part of their separate release/platform lanes.
This capability restriction does not exclude the `vo-release` crate from
Windows workspace tests.

## Workflows

### CI

`.github/workflows/ci.yml` runs for pull requests, merge groups, `main` pushes,
and manual dispatches.

Pull requests use conservative component impact selection:

- repository contracts always run;
- Rust, language, Web, and UI smoke lanes run only when their owned inputs are
  affected;
- UI product changes include Linux smoke and the macOS/Windows platform lanes;
- weighted case sharding keeps every backend variant of a language case
  together while balancing declared timeout cost.

Merge groups and `main` run the complete language matrix, full Wasm/Web suite,
and real Linux, macOS, and Windows UI/AOT matrix. A `main` run additionally
produces the Studio Pages candidate. Superseded pull requests are cancelled;
immutable branch candidates and Nightly runs retain their execution. Rust compilation uses the GitHub sccache backend; dependency caches
contain downloads only and never serve as test evidence.

The Studio candidate has independent raw, gzip, Brotli, and total-precache size
budgets. The raw AOT limit protects browser decode/compile cost; the compressed
limits protect first-load transfer cost. Browser smoke also enforces startup
timing, so a size-compliant image cannot silently regress into a slow product.

### Nightly

`.github/workflows/nightly.yml` runs release-mode native and Wasm/Wasm-AOT
language matrices, GC/JIT/OSR/scheduler stress selections, macOS and Windows
workspace tests, bounded protocol fuzzing, and Rust/npm audits. It emits and
certifies the same task receipts as CI.

Cross-repository Voplay fuzzing is excluded from the core Nightly contract. Its
standalone harness remains under `fuzz/voplay-protocol` for a workspace where
the exact `eng/project.toml` Voplay revision is present. A sibling repository
cannot make the Volang core gate fail merely because it was absent from the
checkout.

### Site

`.github/workflows/site.yml` starts only after a successful `main` CI run, or
from an explicitly selected successful main run. It downloads that run's
certification and Studio candidate, verifies the commit and recursive artifact
digest, rechecks deployment budgets, and uploads those exact bytes to Pages.
It performs no compiler, runtime, or application rebuild.

After deployment, a public smoke probe checks the shell and non-empty
`app.wasm`. The Pages environment remains the sole deployment authority.

### Release

`.github/workflows/release.yml` accepts an existing `v*` tag through the
default-branch `repository_dispatch` entry. Preflight verifies tag identity,
main reachability, protected release policy, and the exact successful main CI
bundle. That bundle must contain full Web, Linux, macOS, and Windows UI
evidence.

The browser VM/Core Wasm runtime is built once and shared by every target job.
Target jobs build and smoke-test Linux x64/arm64, macOS x64/arm64, and Windows
x64 archives. Each archive provenance record binds the product-certified CI
bundle digest and commit. Publication re-verifies all archives and the bundle,
creates GitHub build-provenance attestations, then crosses the protected
`release` environment.

## Local verification

Use Rust 1.94.0, Node 24, and wasm-pack 0.14.0. Run from the repository root:

```sh
export VOWORK=off
cargo fmt --all -- --check
cargo run -q -p vo-dev --locked -- lint all
cargo test --locked -p vo-dev
```

Inspect plans without creating trusted evidence:

```sh
cargo run -q -p vo-dev --locked -- ci plan \
  --profile pull-request \
  --changed-file ui/ui.vo \
  --output target/ci/plan.json
cargo run -q -p vo-dev --locked -- ci plan \
  --profile main \
  --output target/ci/main-plan.json
```

Representative language commands are:

```sh
cargo run -q -p vo-dev --locked -- test run \
  --suite lang --tags smoke --targets native,gc-vm,gc-osr,compile
cargo run -q -p vo-dev --locked -- test run \
  --suite lang --targets native,gc,embed,compile --shard 1/2
cargo run -q -p vo-dev --locked -- test run \
  --suite lang --targets wasm,wasm-aot --release
```

Web checks use the locked npm workspace:

```sh
npm --prefix lang/crates/vo-web ci
npm --prefix lang/crates/vo-web run test:vfs
npm --prefix lang/crates/vo-web run test:ui
npm --prefix lang/crates/vo-web run test:ui-browser
```

Browser scenarios use the exact Playwright version and Chromium revision in
`eng/browser/package-lock.json`. The compatibility entry point preserves all
eight original product scenarios and adds Studio startup and account-free
canary regressions, with their assertion mapping in
`eng/browser/coverage.json`. Each attempt retains failure traces, screenshots,
console/network diagnostics, and HTML/JSON reports under `target/ci/browser`.
Retries are disabled; `eng/browser/check-diagnostics.mjs` verifies controlled
failures for every registered scenario. See `eng/browser/README.md` for local setup.

Dependency audits are:

```sh
node eng/run-dependency-audit.mjs
```

`eng/dependency-policy.json` declares the root and two maintained fuzz lockfiles,
plus the Web and browser-tool npm workspaces. Every Rust report records an
unfiltered advisory database revision. High/critical npm findings and Rust
vulnerabilities fail the lane. Informational Rust findings require an exact
package/version/advisory/lockfile review with an owner, dependency chain and
expiry of at most 90 days. Expired or new findings fail. The current 13 warning
reviews include the transitive GTK3/glib migration; the glib unsoundness remains
a tracked risk and has a 30-day review window. Raw reports, process exits and
timings accompany `target/ci/results/dependencies.json`, including on failure.

If the local Cargo output tree has accumulated years of profiles and target
triples, metadata scans can dominate command startup. `vo-dev clean rust`
removes that cache deliberately; a temporary `CARGO_TARGET_DIR` is useful for
isolated diagnostics. For tests that launch nested Cargo builds, prefer Cargo's
`--target-dir` option so the child fixtures retain their own isolated output
directories. CI relies on clean runners plus sccache and never uploads the whole
`target` tree.

## Required repository settings

Repository configuration must enforce these controls before release:

1. Protect `main` and require pull requests, one approving review, resolved
   conversations, linear history, and the `CI / required` check. Block force
   pushes and deletion.
2. Enable merge queue if maintainers use merge groups; require the same stable
   check there.
3. Enable private vulnerability reporting, dependency graph, Dependabot
   alerts/security updates, secret scanning, and push protection.
4. Enforce HTTPS for Pages and protect the `github-pages` environment.
5. Protect `v*` tags, enable immutable releases, and configure a `release`
   environment with independent reviewers, self-review disabled, administrator
   bypass disabled, and an explicit `main` branch policy.
6. Store `RELEASE_SETTINGS_TOKEN` only in the release environment. It requires
   repository Administration read access and is used solely to fail closed on
   immutable-release policy.

These settings live on GitHub and need administrator application. Repository
files provide CODEOWNERS, dependency-update policy, protected workflow logic,
and the auditable target state.
