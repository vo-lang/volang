# Continuous integration and delivery

Volang uses one declared CI task model, source-bound evidence, and promotion of
tested bytes. The machine-readable sources of truth are:

- `eng/ci.toml` for pull-request, merge, main, and Nightly task profiles;
- `eng/tests.toml` and `tests/lang/manifest.toml` for language cases and costs;
- `eng/release.toml` for release targets and archive policy;
- `eng/toolchains.toml` and `rust-toolchain.toml` for pinned tools.

Workflow YAML provisions and schedules lanes. Repository contracts, Rust quality,
language, Web, native UI and dependency checks execute ordered command definitions
from `eng/ci.toml` through `vo-dev ci run`. `vo-dev ci lint` validates the task graph,
dependencies, owners, platforms, budgets, runners, and safe evidence paths.
Actionlint validates workflow syntax and expressions with ShellCheck for embedded
scripts. GitHub CI downloads both declared versions with verified archive hashes.
Local workflow validation also requires ShellCheck on `PATH`; the task fails
explicitly when it is missing, so a syntax-only check cannot stand in for the
complete workflow check. Versions are listed in `eng/toolchains.toml`.

## Trust model

Each CI run follows the same chain:

1. `vo-dev ci plan` selects an immutable task set and records the source commit,
   Git tree, profile, changed paths, and complete task definitions. Impact plans
   also bind resolved base/head/merge-base object IDs and a component graph digest.
2. Each job calls `vo-dev ci run --plan <path> --task <id>`. Its receipt
   binds the task, source, CI plan, toolchain files, test manifests, runner,
   GitHub run/job identity, timing, result files, and promotable artifacts. Domain results must identify a complete test or browser
   scenario; arbitrary success flags are rejected. Every declared task requires
   execution, and rejects the legacy `ci record` entry point.
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
group or Windows Job Object. Workflow jobs reserve a further 15 minutes for
provisioning and diagnostic uploads; the quality job includes both sequential
task budgets. These outer allowances do not change test or task deadlines.
Cancellation uses the same cleanup path. Commands
run once using their declared argument vector and environment. Commands that
explicitly select Bash use fail-fast scripts; the executor never implicitly
expands an argument through a shell. Automatic retries remain disabled. Platform Nightly lanes
run a byte-checked copy of `vo-dev` from `target/ci/bin`, allowing workspace
Cargo tests to replace `target/debug/vo-dev.exe` while the executor remains alive
on Windows.

All Nightly language, stress, platform, fuzz and audit lanes use `ci run`.
Their executable arguments and budgets live in `eng/ci.toml`; Actions installs
the environment and selects a static task. Language JSON is published from the
child's exact stdout only after schema validation, and its digest must match
the captured stdout. Fuzz commands require both completion records to match
the declared positive input budget. Failed attempts retain logs and Cargo timings.

`started.json` explicitly marks an incomplete attempt. The executor atomically
writes the typed `result.json` and its digest in `completion.json`, then publishes
certifiable evidence as its final commit point. Certification errors update
the typed result and completion digest, write `failure.json`, and cause a
nonzero exit. Local dirty executions can
produce diagnostic receipts, but cannot produce certifiable evidence. Bundles
reject mixed GitHub run attempts and missing command or log records.

Rust test commands require a nonempty successful test set, independently of the
process exit code. Failure records identify the command, owner through the task
definition, classification, reproduction command and diagnostic paths. Job
summaries show per-command test counts and durations; Cargo HTML timings are
uploaded with executor diagnostics. Workspace tests continue across failing
test programs so one run reports all failures while preserving a failing exit.

Each attempt atomically refreshes a bounded display-only task summary at start,
command boundaries and completion. The required job collects these small files
before rejecting failed lanes. `vo-dev ci summarize --plan <path> --summaries <dir>`
shows the first recorded failure, owner, declared command, reproduction and log
location, plus all task states. Missing, stale, oversized or mismatched summaries
are visibly invalid; interrupted attempts remain incomplete. Summary identity
binds source, plan, task and GitHub run attempt. Summaries have no certification
authority and their rendering cannot turn failed jobs into successful checks.

Certification artifacts (`ci-evidence-*` and `nightly-evidence-*`) contain only
the compact evidence files. Execution logs, per-case Native AOT receipts and
Cargo timings travel in separate diagnostic artifacts, including failed attempts.
The certification collector keeps its bounded scan and rejects incomplete
coverage; adding thousands of diagnostic files cannot exhaust that scan.

Nightly's browser and release Wasm language tasks share one static Web job and
execute sequentially. Each retains its own immutable task receipt and deadline;
the second task runs after a failure unless the job is cancelled. Both use the
same Cargo output tree, release Wasm compiler configuration and locked Node
dependencies. Build commands still check freshness through Cargo. The language
task explicitly installs its locked JavaScript tools for isolated reproduction.
The combined job reserves both task deadlines plus one provisioning allowance.
This scheduling choice follows [Nightly measurements](https://github.com/vo-lang/volang/actions/runs/33995230524):
the separate Web jobs took 816 and 714 seconds, below the 2712-second Windows
critical path even when added sequentially. Whole Cargo output trees remain
uncached across runs.

Embedded standard-library assets use deterministic zero timestamps. Their source
bytes still come from the canonical `lang/stdlib` tree; host checkout times and
`SOURCE_DATE_EPOCH` do not enter the embedded metadata. This keeps separately
built site and release Web runtimes comparable by byte digest, including when
compiler caches reuse the embedding proc macro's output. Native debug source
editing continues to read live files through the source API.

Both Web profiles execute their ordered commands through `ci run`. The full
profile retains all 31 semantic probes and tests the final Studio directory
through the complete journey, startup, canary and offline lifecycle contracts.
Image and precache budget results bind their measured artifacts, complete asset
lists and consistent limits. These compatibility checks use
`target/ci/artifacts/legacy-studio`; they no longer produce the default site.
The separate `ui-web-rewrite` task seals `target/ci/artifacts/site` in its own
execution receipt. Site promotion verifies that producing task's artifact digest;
no additional task infers success from the presence of a directory.

The replacement desktop framework has three independent tasks:
`ui-desktop-rewrite-linux`, `ui-desktop-rewrite-macos`, and
`ui-desktop-rewrite-windows`. Pull requests select them through desktop source
impact; merge and main include all three. They build the release compiler,
execution-only Wasm runtime and optional Studio compiler, then a matching native
SDK. The first Wasm build may install the exact binding tools selected from
Cargo.lock; subsequent builds reuse them. SDK builds run offline after an
explicit locked dependency fetch. Linux installs WebKitGTK 4.1/GTK 3 and executes
system windows under Xvfb. Windows uses WebView2 and the compiler's MSVC discovery.

Each task drives the public CLI through a relocated toolchain and independently
relocated VM/JIT/Native AOT applications. The native runtime assertions prove JIT
and AOT entry, including zero JIT compilation in AOT. The Studio driver separately
checks all three native backends, offline documents, optional editor, compiler
workers, cancellation, draft recovery and interactive previews. The aggregate
rejects missing or duplicated scenarios, mixed platform/profile results and
changed executable or bundle receipts. The development preview additionally
checks ordinary and popup HTTP(S) navigation through a recording callback, then
continues real application interaction; CI never opens an external browser.
macOS has 52 delivery assertions plus 36 Studio assertions; Linux/Windows have
49 plus 36, with no macOS plist checks. Six delivery assertions package and run persistent storage across reopening,
relocation and a second application identity. Four delivery assertions cover building
that preview and bytecode, then its VM and JIT window contracts.
These are system WebView DOM assertions; physical input, paint, IME and assistive
technology acceptance remain separately tracked.

The final result is `target/ci/results/ui-desktop-rewrite.json`. Each task keeps
its raw reports and SDK under `target/ci/artifacts/ui-desktop-rewrite`; successful
uploads include both reports and `desktop-sdk.tar.gz`, whose `sdk/` directory
preserves native executable permissions. Failures retain the process logs and
partial reports. `VO_UI_DESKTOP_SDK` selects an SDK for checkout-only drivers;
packaged tools always use their inventoried SDK. CI owns its SDK output directory
and archives earlier attempts before rebuilding it. These preview tasks retain
separate identities from the previous native product's compatibility checks.

The experimental Web rewrite runs as `ui-web-rewrite` in the independent
`ui_web_preview` job. Pull requests select it through source impact; merge and
main profiles include it. Its declared prerequisites build the JIT-enabled native
compiler in the release profile, isolated browser compiler, execution-only Wasm VM
from locked source. The default browser package is built separately for legacy
compatibility probes. The job installs the locked Playwright versions of Chromium,
Firefox and WebKit.

`node eng/ui-next/ci.mjs` executes the bounded core gate after those prerequisites
are available: all top-level Node unit contracts, native VM/JIT contracts, fresh
application images and three browser matrices, the relocated native CLI's three
public templates and optional editor, source reload and compile-cache recovery,
and a fresh Studio distribution with compressed delivery and offline workers.
The locked VS Code authoring dependencies build a self-contained VSIX in each
portable toolchain. Its packaging contract checks reproducibility, and the moved
CLI's LSP session checks completion, UI source definitions, Unicode positions,
type errors, successful warnings and captured standard-library sources against a
real generated project. These receipts bind the native compiler digest. Actual
VS Code host checks run separately with `npm --prefix ui/editors/vscode test`;
their platform coverage is reported independently from browser coverage.
The public host contract packs `vo-web` using its normal prepack hook and installs
the archive offline in an independent consumer. Strict TypeScript resolution,
browser bundling and 12 browser cases cover both VM distributions,
SSR/client rendering, simultaneous roots, provider cleanup and remounting.
Archive contents, fixture images, source inputs and browser versions are bound
to the receipt; incomplete runtime coverage fails aggregation. Packaging uses
a fresh staging directory so an earlier local prepack cannot hide missing assets.
The mount boundary also checks queued SSR actions throughout artifact/runtime
loading, repeated root ownership, failed startup/retry and native form resets.
The portable-toolchain regression extracts both complete guide applications from
the delivered toolkit; migration tests queue input and Save before activation.

It then exports the static Studio, relocates it outside the checkout, and checks
all three engines and both backends, including direct pages, hydration, browser
compilation, the optional editor and project recovery. Static evidence is bound
to the exact native distribution that produced its pages. The upgrade check
keeps the previous Studio worker and an unsaved tab alive across an atomic local
deployment. All three engines must preserve project files, drafts and unrelated
caches/registrations, retire the owned cache, follow old URLs and recover from a
failed VM startup. Its receipt binds the previous-worker fixture and exported
site; partial upgrade coverage fails aggregation. The core command uses
`VO_TEST_PROFILE=release` to select the tested native CLI and packaged executable.
The broader `node eng/ui-next/cli.mjs check` also runs specialized project and
server scenarios; those additional drivers are not implied by the core receipt.

The aggregate rejects partial backend coverage and mismatched application,
Studio or toolchain inventories. The browser compiler/runtime JavaScript and Wasm
files are bound to the application build, checked before and after each browser
matrix, and matched to the delivered Studio files. It copies raw reports to
`target/ci/artifacts/ui-web-rewrite/reports`, records
`target/ci/results/ui-web-rewrite.json`, and packages the verified tools as
`target/ci/artifacts/ui-web-rewrite-toolchain.tar.gz`. Tar preserves the native
compiler's executable mode. The verified static site is archived separately as
`target/ci/artifacts/ui-web-rewrite-studio-static.tar.gz`; its extracted root is
ready for an origin-root directory-index host. Per-step logs remain under `target/ui-next/ci` and
are uploaded on failure. The following site staging command preserves every
static application byte and its build report, adds domain metadata, and writes
`target/ci/artifacts/site`. A separate check verifies its complete inventory,
deployment budgets and served application files before and after the shared
Gallery/Docs/Playground journey on the Web VM. Its result is
`target/ci/results/ui-web-site/report.json`; it belongs to the same task receipt.

These artifacts retain experimental status: this gate does not establish a
formal performance baseline, device/assistive-technology acceptance or product
certification. Hosted Linux evidence is separate from local macOS execution.

Native UI tasks retain eight real window scenarios on each full platform and
two Linux smoke scenarios, alongside Rust contracts and VM/JIT differentials.
Each window must emit its exact ordered semantic clicks, successful final text
assertions and presentation boundary. Exit zero alone cannot satisfy that
contract. Packaged applications are sealed under `target/ci/artifacts/native`;
their receipts and timing logs share the same task attempt.

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

The explicit `native-aot` language target builds the compiler and core runtime
once per profile, then compiles, links and executes each selected program in
an isolated case directory. At most two AOT cases may link concurrently. Each
case retains phase logs and a receipt with runner, compiler, runtime and executable
digests. After each worker and its process wrapper exit, the coordinator verifies
the executable's retained bytes against the receipt and removes it to bound disk
use. Verification or cleanup errors are infrastructure failures and preserve the
file for diagnosis; cleanup has no retries. Build or execution failures also
retain any executable produced.
The debug profile optimizes the SHA-256 helper with debug assertions and overflow
checks enabled. Debug executable receipts can cover hundreds of megabytes, so
verification must remain practical on CPUs without SHA instruction acceleration;
the compiler, runtime and test runner retain their debug profiles.
The merge Native AOT smoke lane uses the release profile, matching the nightly
AOT lanes. Large imported packages otherwise spend most of their case deadline
in unoptimized native code generation; the deadline and VM differential remain
unchanged. Debug AOT execution remains available through `vo-dev test run`.
Differential failures retain the logs and executable digest. The case deadline includes
both build and execution, and process-group cleanup also terminates descendants.
Successful program output is compared with the matching VM case when present.

Compile-negative cases exercise the actual AOT build command independently.
For host-specific build rejection, a passing language case may declare
`expect_by_target = { native-aot = { fail = ["diagnostic"] } }`, with an owner
and reason. The planner retains its VM contract and requires the selected AOT
build to fail with every declared diagnostic and no executable. Such a result
proves rejection, and carries no execution-equivalence claim. The default AOT
runtime's compiler-host rejection is covered through this contract.
The `native-aot-host` target explicitly builds the runtime's `toolchain-host`
feature. Nightly pairs it with VM execution for the `compiler-host` cases on
Linux, macOS and Windows. Mixing core and compiler-host AOT targets in one
invocation fails before building, so the core rejection cannot accidentally
use a runtime with the extra capability.

Set `VO_UI_PACKAGE_TIMINGS=1` when measuring `vo ui package`. Its stderr includes
a `volang.ui-package-timings.v1` record for source compilation, native AOT
lowering, linking and final packaging, plus compiler path, assertion mode, total duration and
failure state. Runtime compilation is measured separately through Cargo timings.
Compare matching source, target, runtime and build settings before changing
compiler profiles, sharing outputs or increasing package concurrency.

Full native UI tasks build the optimized packaging compiler and both static
runtimes in one declared Cargo invocation, then package Studio and the four
showcases with explicit runtime paths. The existing debug VM/JIT window probes
remain separate commands. This shared invocation is a distinct configuration:
[Cargo can unify dependency features](https://doc.rust-lang.org/cargo/reference/features.html#feature-unification)
and change archive bytes, so reuse must
include the complete package/feature selection and the sealed artifact digests.
The five native user journeys validate the resulting packages on each platform.

Manual CI and Nightly runs accept `cold_cache=true` to disable compiler caching
in every lane, including planning and certification. Task commands and deadlines
remain identical. Dependency and browser-tool downloads may still be cached;
Cargo compilation outputs are built afresh. Use this mode for the cold compiler
cache acceptance run instead of deleting shared caches.

Native filesystem ordering always runs. The symlink regression independently
probes the host using Rust before invoking Vo. Supported hosts exercise relative
file and directory links, absolute targets, dangling links and ReadDir/Lstat
metadata. Unavailable hosts must return an error and leave no entry. Cases
that require this capability declare `requires_host = ["symlink"]`; an unavailable
required capability produces a typed portability failure before execution.

Windows Nightly compiles the complete standard library before preparing AOT
tools, so conditional compilation errors fail early. Pipe regressions require
normal byte transfer and an explicit write error after the reader closes on
Linux, macOS and Windows.

Animation contracts use an explicitly installed per-VM manual clock shared by
stdlib time reads and timer completions. They test intermediate values,
cancellation, completion and actual JIT execution without sleeping.

Wasm VM language cases run in a bounded Node worker pool. The parent compiles
one WebAssembly module and each case creates a fresh instance, VM, VFS and
environment. The default is at most four workers; the direct runner accepts
`--jobs 1..8`. A case deadline terminates its worker, including synchronous Wasm
loops, before that slot admits another case. Worker crashes, missing or duplicate
results, and excessive diagnostic output fail the case. Reports retain plan
order regardless of completion order. Supervisor contracts exercise actual
worker and Wasm termination as part of the Web test suite.

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

The new Studio candidate has independent raw and delivered gzip/Brotli limits
for the VM application, execution runtime and optional browser
compiler, plus a complete directory size limit. The policy is owned by
`eng/ui-next/studio-site-budgets.mjs` and measures the actual gzip-6/Brotli-4
siblings. Missing representations fail verification. These are size regression
guards, separate from the unresolved interaction budgets documented in
[the framework performance report](../ui/next/performance.md). The previous
Studio's precache and image budgets remain in its compatibility task.

### Nightly

`.github/workflows/nightly.yml` runs release-mode native and Wasm VM
language matrices, GC/JIT/OSR/scheduler stress selections, macOS and Windows
workspace tests, bounded protocol fuzzing, and Rust/npm audits. It emits and
certifies the same task receipts as CI. The macOS task also runs the repository
AppKit lifecycle probe, observing show, resize, minimize, restore and close
without fixed settling sleeps; it requires typed complete event evidence.

Cross-repository Voplay fuzzing is excluded from the core Nightly contract. Its
standalone harness remains under `fuzz/voplay-protocol` for a workspace where
the exact `eng/project.toml` Voplay revision is present. A sibling repository
cannot make the Volang core gate fail merely because it was absent from the
checkout.

### Site

`.github/workflows/site.yml` starts only after a successful `main` CI run, or
from an explicitly selected successful main run. It downloads that run's
certification and Studio candidate, verifies the commit and the recursive
`ui-web-rewrite` artifact digest, rechecks deployment budgets, and uploads those
exact bytes to Pages, including hidden hosting files.
It performs no compiler, runtime, or application rebuild.

Before and after deployment, `studio-site-cli.mjs check` uses pinned Chromium
to exercise Gallery, Docs and Playground with Wasm VM. It compares every
served application file against the certified directory around the journey.
CNAME and `.nojekyll` remain bound locally; HTTP availability is not required
for these hosting metadata files. Cancellation joins requests and closes the
browser and local server. Reports stay outside the candidate directory.

The Pages environment remains the deployment authority, and superseded main
candidates cannot deploy. For Actions-based Pages publishing, domain settings
are owned by the repository's Pages configuration; CNAME alone does not change
them ([GitHub domain documentation](https://docs.github.com/en/pages/configuring-a-custom-domain-for-your-github-pages-site/managing-a-custom-domain-for-your-github-pages-site)).

### Release

`.github/workflows/release.yml` accepts an existing `v*` tag through the
default-branch `repository_dispatch` entry. The tag must equal the main commit
that triggered the workflow, so GitHub's OIDC provenance and the checked-out
release source identify the same commit. Preflight verifies tag identity,
main reachability, protected release policy, and the exact successful main CI
bundle. That bundle must contain full Web, Linux, macOS, and Windows UI
evidence.

The browser VM runtime is built once and shared by every target job.
Target jobs build and smoke-test Linux x64/arm64, macOS x64/arm64, and Windows
x64 archives. Each archive provenance record binds the product-certified CI
bundle digest and commit. Inside the protected `release` job, publication
re-verifies all archives and the bundle, creates GitHub build-provenance
attestations, and verifies each archive's signed source commit, workflow commit,
main ref and hosted runner identity before publishing. Signed bundles and
verification results are retained even when a later publication step fails.

The native archives also carry the complete experimental Web project toolkit at
`share/volang/ui-next`. Each target packages its own locked Node dependencies and
matching CLI; the shared build supplies the separate execution-only Wasm runtime.
Build receipts and archive provenance use schema 8 / `tar+gzip-v5`, recording every
toolkit file's size, digest and normalized executable mode. The copied toolkit
compiler must match the top-level CLI byte for byte. Keeping the standalone
toolkit layout makes relocation and verification use the same implementation;
it currently includes a second copy of that CLI. The compatibility runtime stays
in `share/volang/ui-web`, and its existing certification remains separate from
the experimental replacement's status.

Every target job extracts its actual archive, resolves the project tools through
the installed `vo`, verifies the inventory, creates a fresh project and runs its
Chromium VM browser tests. Node.js 24 and the pinned browser dependencies are
installed explicitly. Archive publication still follows the protected workflow
above; adding toolkit contents does not confer new platform or product certification.

For a local archive rehearsal after the complete UI gate, build the native static
runtimes and use the verified `target/ui-next/ci/toolchain`. The explicit ignored
test `release_archive::installation::local_installation_archive` reads absolute
`VO_RELEASE_UI_TOOLCHAIN` and a new `VO_RELEASE_INSTALL_PROBE` output directory.
Run it with `cargo test -p vo-dev --locked` and
`-- --ignored --exact --nocapture`. It uses the production archive writer and
verifier, then retains the extracted installation and per-input digests. Exercise
that installation with `vo ui verify`, `vo ui create`, and `vo ui test --project`
outside the checkout, with `VO_UI_TOOLCHAIN` unset. This local rehearsal neither
publishes assets nor substitutes for a clean candidate and hosted platform CI.

Manual dispatch rehearses the same five targets using a successful full CI run
for the exact selected branch commit. `release candidate metadata|matrix|build|package|verify`
uses an explicit candidate identity, no release tag, a `vo-candidate-` archive
prefix, and `target/ci/release-candidate/<commit>` output. Candidate evidence must
use the full `merge` profile; production continues to require the tagged `main`
identity. Build receipts and archive provenance use schema 7 and bind that purpose
and the declared CLI build arguments, optimization level and LTO configuration.
Candidate mode exposes no publication or Homebrew operation. The workflow's
production publish job runs only for `repository_dispatch`.

Each freshly built CLI must execute `--version` within 60 seconds and return the
exact version, full source commit and commit date. A typed probe preserves its
output and exit status. The executable digest must remain unchanged during the
probe and through build receipt creation; archive provenance binds those same
bytes. Identity validation does not depend on the compiler's string layout.

Every target tests VM/JIT and a linked Native AOT executable, unpacks its archive,
builds a Web UI application, creates and tests a starter project, then links and
opens a real native UI window using the packaged runtime. The window probe has a
120-second process-tree deadline and requires the exact presentation record;
it saves a typed result with executable digest, source, exit status, timing and
logs. Linux uses Xvfb and Mesa. Build failures preserve Cargo timings and receipts.
After all five targets pass, candidate verification checks the complete archive
set, provenance and digests, creates GitHub Artifact Attestations, and verifies
their source commit, ref and signing workflow. This rehearsal produces candidate
artifacts and verification records; it grants no production publication authority.

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
  --suite lang --targets wasm,wasm --release
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
canary regressions plus a Nightly offline/recovery/resource-cleanup journey,
with their assertion mapping in
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

## Site promotion journeys

Site promotion downloads the exact main CI site and certification, verifies its
digest and budgets, runs the complete Studio journey against that final directory,
and verifies the digest again before uploading Pages bytes. Deployment is serialized;
a candidate superseded on main is skipped before the Pages action runs.

After deployment, the pinned Chromium canary first checks all executable and user
content assets (HTML, JavaScript, Wasm, JSON, images, CSS and fonts) against that
same certified artifact, with bounded downloads and no retries. It then creates a
fresh browser-local project, edits and runs code, saves, clicks the UI preview and
checks refresh/reopen persistence. Failures retain Playwright traces, screenshots
and domain results. Pages configuration, private build markers, TypeScript declarations
and source maps are outside the public asset comparison. The complete artifact
including those files remains bound by the promotion certificate.

### Language host contracts

Language plan/result v2 binds the physical host OS separately from its execution
backend. The manifest declares supported `platforms`, `requires_host` and an
optional `resource_group`; defaults preserve unrestricted platform selection.
Planning reports platform exclusions before sharding. Actual runners reject
foreign-host plans, probe required host capabilities independently, and return
failed jobs when a prerequisite is unavailable. Capability observations and
failure categories survive JSON aggregation and task diagnostics. Certification
checks that the domain host matches the producing task runner.

Resource groups reserve case groups within each plan without occupying workers
waiting for busy resources. Native AOT retains its independent two-linker bound;
Wasm VM retains a bounded worker pool. No automatic test retries are added.
