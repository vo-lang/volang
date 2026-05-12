# CLI, Tests, CI, And Benchmarks

## Contents

- [Main CLI](#main-cli)
- [Developer Script](#developer-script)
- [Engineering Control Plane](#engineering-control-plane)
- [Test Runner](#test-runner)
- [CI Tasks](#ci-tasks)
- [Benchmarks](#benchmarks)
- [Validation Selection](#validation-selection)
- [Environment Notes](#environment-notes)
- [Caveats](#caveats)

## Main CLI

Main entry: `cmd/vo/src/main.rs`.

Commands:

- `vo run <file|dir> [--mode=jit] [--codegen] [-- args...]`
- `vo build [path] [-o out]`
- `vo check [path]`
- `vo test [path] [--mode=jit]`
- `vo fmt [file|dir...] [--check]`
- `vo init <module-path>`
- `vo mod <subcommand>`
- `vo release <subcommand>`
- `vo emit <file|dir> [-o out]`
- `vo dump <file.vob>`
- `vo version`

Important distinctions:

- `vo test` compiles and runs a project/test entry. It is not the same as the repo language regression runner.
- `vo build` and `vo emit` write serialized `.vob` bytecode.
- `vo run --codegen` prints bytecode text and does not run.
- JIT mode is selected with `--mode=jit`; `d.py` may set `VO_JIT_CALL_THRESHOLD=1` for direct JIT experiments.

## Developer Script

`./d.py` is the preferred local wrapper. It must run from repo root.

Common commands:

- `./d.py test [target|alias] [--release] [-v] [-j N|-jN] [--repeat N|-n N] [file]`
- `./d.py run <file.vo> [--mode=vm|jit] [--codegen]`
- `./d.py vo <args...>`
- `./d.py gc-perf ...`
- `./d.py bench [all|vo|<name>|score] [--all-langs]`
- `./d.py loc [--with-tests]`
- `./d.py clean [all|vo|rust|bench|junk]`
- `./d.py studio [--build-wasm] [--build-only] [--runner] [project]`
- `./d.py studio-native [--build-wasm] [--runner] [project]`
- `./d.py studio-stop`
- `./d.py ci ...`

`d.py` is a thin compatibility dispatcher. Its only job is repo-root validation and `cargo run -q -p vo-dev -- dpy ...`; `d_py.py` has been removed. The compatibility parser lives in `cmd/vo-dev/src/dpy_compat.rs`, reads test selector names from `eng/tests.toml`, and keeps wrapper-level `--release` limited to commands that document it, such as `test` and `gc-perf`. Forwarded `vo`/default commands keep their original arguments.

`./d.py clean junk` removes local-only clutter such as `.DS_Store`, `__pycache__`, `.pyc`, `.volang/tmp`, and the repo-root `.tmp/` log/scratch directory without touching Rust targets or Node installs. `./d.py clean vo` removes repo-local Vo cache state under `.volang/cache/vo`, old `.vo-cache` leftovers, and simple bytecode outputs. `./d.py clean bench` removes benchmark result files and benchmark build products (`go_bench`, `c_bench`, `.class`). `clean all` combines junk, Vo, benchmark, and Rust cleanup.

## Engineering Control Plane

Engineering-system commands are owned by `cmd/vo-dev`.
Use it through Cargo unless a local binary has already been built:

- `cargo run -q -p vo-dev -- task list`
- `cargo run -q -p vo-dev -- task show <task>`
- `cargo run -q -p vo-dev -- task plan <selector> [--changed] [--format text|json]`
- `cargo run -q -p vo-dev -- task run <selector>`
- `cargo run -q -p vo-dev -- ci matrix pr --github-output`
- `cargo run -q -p vo-dev -- tool check [--task <task>] [--json]`
- `cargo run -q -p vo-dev -- tool bootstrap [--task <task>] [--apply]`
- `cargo run -q -p vo-dev -- verify plan <selector>`
- `cargo run -q -p vo-dev -- verify run <selector>`
- `cargo run -q -p vo-dev -- first-party path <repo> [subdir]`
- `cargo run -q -p vo-dev -- first-party ci-checkout <repo> [--github-output]`
- `cargo run -q -p vo-dev -- first-party run <repo> <subdir> -- <command...>`
- `cargo run -q -p vo-dev -- first-party run-workspace <repo> <workspace> -- <command...>`
- `cargo run -q -p vo-dev -- first-party release-verify <repo>`
- `cargo run -q -p vo-dev -- bench [all|vo|score|<name>] [--all-langs]`
- `cargo run -q -p vo-dev -- loc [--with-tests]`
- `cargo run -q -p vo-dev -- clean [all|vo|rust|bench|junk]`
- `cargo run -q -p vo-dev -- studio [--build-wasm] [--build-only] [--runner] [project]`
- `cargo run -q -p vo-dev -- studio-native [--build-wasm] [--runner] [project]`
- `cargo run -q -p vo-dev -- studio-stop`
- `cargo run -q -p vo-dev -- lint all`

Canonical engineering data:

- `eng/README.md`: ownership and boundary map for the engineering-system data files.
- `eng/tasks.toml`: task graph, groups, dependencies, inputs, outputs, tool requirements.
- `eng/ci.toml`: changed-file planning defaults and fallback behavior.
- `eng/toolchains.toml`: Rust/Node/npm/wasm-pack/Python requirements.
- `eng/project.toml`: first-party sibling repositories and cache policy.
- `eng/artifacts.toml`: checked-in generated artifact policy.
- `eng/release.toml`: release targets, runner OSes, package naming, cross version, release notes, and Homebrew formula policy.
- `eng/tests.toml`: test target defaults, aliases, environments, backends, and timeouts.
- `tests/lang/manifest.toml`: language test cases, skips, expected failures, no_std/embed compatibility.

`./d.py ci ...` delegates directly to `vo-dev`. `scripts/ci` contains only quickplay Node helpers; self-check, first-party lookup, first-party command execution, and release verification are owned by `vo-dev`.

Inside `cmd/vo-dev`, `main.rs` is only top-level dispatch and repo-root discovery. `dpy_compat.rs` owns the `./d.py` compatibility surface, including old test shorthand parsing and default `vo` forwarding. `dev_bench.rs` owns `vo-dev bench`, benchmark dependency checks, release `vo` builds, `hyperfine` invocation, benchmark result parsing, and score calculation. `dev_clean.rs` owns `vo-dev clean` junk/Vo/benchmark/Rust cleanup. `dev_loc.rs` owns `vo-dev loc` source/test line statistics. `dev_studio.rs` owns Studio launch/stop, Studio environment shaping, and Studio/vo-web WASM freshness checks. `dev_common.rs` owns shared developer-tool constants and file traversal helpers. `dev_gc_perf.rs` owns GC performance example dispatch. `task_system.rs` owns only the `vo-dev task ...` CLI dispatcher and output shaping. `task_graph.rs` owns task maps, group resolution, dependency expansion, task-scoped tool collection, and task-scoped Node workspace collection. `task_planner.rs` owns `PlanArgs`, changed-file planning, selector planning, and shared Git line helpers. `task_runner.rs` owns task execution, task output validation, task tool readiness checks, and task timeouts. `ci_system.rs` owns `vo-dev ci`, GitHub matrix/metadata shaping, selected-task Node lockfile output, Rust cache workspace output, and CI checkout aggregation. `verify_system.rs` owns `vo-dev verify`, including tool/cwd/repo/command-path readiness reports. `test_config.rs` owns `eng/tests.toml` target/alias parsing, selector expansion, target command validation, and target tool inference. `test_manifest.rs` owns `tests/lang/manifest.toml` schema parsing, expected-failure parsing, manifest/path synchronization, and manifest lint policy. `test_system.rs` owns only the `vo-dev test ...` CLI dispatcher and output shaping. `test_plan.rs` owns `TestArgs`, language-test plan generation, path filtering, and compile-fail job expansion. `test_runner.rs` owns native `vo-test run-plan` dispatch, WASM build/runner dispatch, temporary plan files, and `vo-embed` prebuilds. `release_config.rs` owns `eng/release.toml` policy validation, release artifact naming, and checksum verification. `release_homebrew.rs` owns Homebrew checkout path derivation, formula target validation, release version replacement, and per-target SHA replacement. `release_system.rs` owns the `vo-dev release ...` command dispatcher and external release command execution. `artifact_lint.rs` owns `eng/artifacts.toml` schema/policy, task generator/validator coverage, provenance JSON validation, and artifact size policy. `artifact_repo_lint.rs` owns tracked/ignored artifact checks, generated-directory detection, Git path state collection, and artifact path matching. `command_lint.rs` owns task command tool inference, first-party nested command checks, and `vo-dev test run` task-tool inference. `lint_system.rs` owns the `vo-dev lint ...` dispatcher plus task and repo-boundary lint execution. `lint_policy.rs` owns shared lint helpers for repo-relative paths, structured input references, artifact containment, ASCII slugs, unique value checks, and declared repo names. `tool_lint.rs` owns `eng/toolchains.toml` lint policy, Node workspace validation, and Rust cache workspace validation. `tool_system.rs` owns `vo-dev tool ...`, tool checks/bootstrap plans, and desired-version resolution. `github_output.rs` owns `GITHUB_OUTPUT` file formatting, including multiline-safe values.

Task runner contracts:

- `timeout_sec`, when present, is a hard task timeout. On Unix, `vo-dev` starts the task in a separate process group and kills the group on timeout.
- `platforms` is reserved and currently rejected by lint until platform-aware execution is implemented.
- declared outputs are checked after a successful task exits; directories must exist and be non-empty.
- task lint infers obvious command tools (`cargo`, `node`, `npm`, `wasm-pack`, `python`, `vo-dev`) and requires matching `tools` entries.
- task lint also looks inside `vo-dev first-party run ... -- <command...>`, `vo-dev first-party run-workspace ... -- <command...>`, and `vo-dev studio-install-local-vogui`, so nested npm usage must be reflected in task `tools`.
- tasks that declare `node` must also declare `node_workspaces`; CI matrix and metadata outputs compute npm cache lockfiles from the selected task closure, including first-party workspaces such as `vogui-js`.
- Node tasks for first-party repos must use `vo-dev first-party run-workspace <repo> <workspace> -- npm ...`; task lint rejects raw-path `first-party run <repo> <subdir>` for Node commands.
- first-party subdirectories used as tool workspaces are declared under `eng/project.toml` `[[first_party.workspace]]`; `eng/toolchains.toml` node workspaces with `repo = ...` must match a `kind = "node"` project workspace path, and tasks must declare the matching `node_workspaces` entry.
- task inputs may use structured `external:<repo>` and `module-cache:<repo>` references for generator jobs whose real inputs live outside the repo tree; lint checks those names against `eng/project.toml`.
- `required = false` is reserved for tools outside default task groups. Task-scoped tool checks always fail if a selected task declares a missing or wrong-version tool.
- GitHub matrix jobs always set up Rust because `vo-dev` itself is the Cargo-based task driver, even when the selected task's declared tools are only Python or Node.
- Rust cache workspace lines come from `eng/toolchains.toml` through `vo-dev ci matrix` / `vo-dev ci metadata`; workflow YAML should not duplicate them.
- artifact generator and validator commands must use `["vo-dev", "task", "run", "task:<task>"]`; generator outputs and validator inputs must cover the artifact path.
- generated checked-in artifacts must declare a provenance JSON file inside the artifact directory. `vo-dev lint artifacts` checks that provenance `artifact`, `path`, `inputs`, and generator command match `eng/artifacts.toml`.
- `vo-dev studio` decides whether `vo-web-wasm-build` and `studio-wasm-build` are stale from the corresponding `eng/tasks.toml` inputs and outputs, then runs those tasks instead of carrying separate hard-coded WASM build commands.
- `studio-wasm-build` depends on `apps/studio/public/quickplay/blockkart/**` because `studio_build_id.mjs` includes the checked-in quickplay package in the Studio WASM build id. The quickplay build-id, smoke, and validation helpers derive artifact paths from `apps/studio/public/quickplay/blockkart/deps.json`; helper code must not pin voplay/vogui package versions separately.

## Test Runner

Repo language test source files still live in `tests/lang`.
The executable test manifest lives in `tests/lang/manifest.toml`.

Test runner structure:

- `cmd/vo-dev/src/test_config.rs`: `eng/tests.toml` target and alias parsing, selector expansion, and target command/tool validation.
- `cmd/vo-dev/src/test_manifest.rs`: manifest schema parsing, case path materialization, expected-failure parsing, filesystem sync checks, and manifest lint policy.
- `cmd/vo-dev/src/test_plan.rs`: canonical language test planning, path filtering, and compile-fail job expansion.
- `cmd/vo-dev/src/test_runner.rs`: native `vo-test run-plan`, WASM build/runner dispatch, temporary plan files, and embed prebuilds.
- `cmd/vo-dev/src/test_system.rs`: `vo-dev test ...` CLI dispatch and text/json output shaping.
- `cmd/vo-test/src/main.rs`: native `run-plan` executor, worker pool, per-job subprocess isolation, execution, and result aggregation.
- `cmd/vo-test/README.md`: executor contract.

Direct `vo-test <mode>` selection is intentionally unsupported; use `vo-dev test run` or `./d.py test ...`.

The manifest classifies:

- `.vo` files
- `proj_*` directories
- `.zip` test packages
- target-specific skips
- target-specific timeout overrides with `timeout = { jit = 180 }`
- expected failures and diagnostic patterns
- embed/no_std incompatibilities

Targets include VM, JIT, GC VM/JIT, no_std/embed, compile-only diagnostics, and WASM through `eng/tests.toml`. Aliases such as `both`, `gc`, and `embed`, default test targets, required file/pass coverage targets, and the GC-regression alias are also declared there; `vo-dev dpy test ...` reads selectors from that file and does not carry its own target map. WASM dispatch is based on `kind = "wasm"`, not the literal target name. The WASM target's build and runner commands are also declared in `eng/tests.toml`; `vo-dev test run --targets wasm` must not hard-code `lang/crates/vo-web` paths outside that config.

Task lint treats those target commands as tool dependencies. If a task runs `vo-dev test run --targets wasm`, its `tools` list must include the tools inferred from the WASM build and runner commands in `eng/tests.toml`, such as `node` and `wasm-pack`; otherwise CI setup could drift from the test target definition.

The WASM runner is plan-only: it accepts `--plan <plan.json>` from `vo-dev` and does not accept raw `.vo` path arguments. It consumes the same `volang.test-plan.v1` jobs as native execution, including per-job `env` and `timeout_sec`. It runs normal pass cases through `compileAndRun`, and expected-failure cases through the WASM compiler path by requiring `compile_error` plus the manifest diagnostic patterns. Pattern matching uses the same ordered `X` wildcard segmentation as the native runner. This keeps WASM compiler diagnostics covered without a separate manifest.

Manifest lint policy:

- normal file/pass cases must include every `required_file_pass_targets` entry from `eng/tests.toml`; omitting any of those targets requires a reason unless the case is a GC-specific regression.
- GC-named regressions must target every target from the `gc_regression_alias` selector in `eng/tests.toml`.
- non-blank cases may not skip every declared target.
- blank placeholders must set `blank = true`, skip each declared target, and include a reason.
- expected failures must have no runtime targets, must include diagnostic patterns, and must include a reason.

Native `vo-test` defaults to at most 8 workers when `--jobs` is omitted. Use
`--jobs N` only when you intentionally want a stronger local stress run or a
more constrained focused repro. Text mode prints a 30-second progress heartbeat
and repeats all failures at the end.

Prefer manifest-aware broad runs:

- `cargo run -q -p vo-dev -- test lint --suite lang`
- `cargo run -q -p vo-dev -- test plan --suite lang --targets vm,jit --format json`
- `cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit`
- `cargo run -q -p vo-dev -- test run --suite lang --targets compile`
- `cargo run -q -p vo-dev -- test run --suite lang --targets nostd`
- `cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit --format json` for structured native results. The JSON root object has schema `volang.test-result.v1`. `--format json` is not supported when the selected targets include `wasm`.

Use manifest-aware focused test files when possible:

- `cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit --path tests/lang/cases/foo.vo`
- `cargo run -q -p vo-dev -- test run --suite lang --targets jit --path tests/lang/cases/foo.vo --verbose`
- `cargo run -q -p vo-dev -- test run --suite lang --targets wasm --path tests/lang/cases/foo.vo`
- `./d.py test both tests/lang/cases/foo.vo`
- `./d.py test jit tests/lang/cases/foo.vo`
- `./d.py test wasm tests/lang/cases/foo.vo`

Use `--repeat` / `-n` for flaky runtime or scheduler bugs.

## CI Tasks

Task graph: `eng/tasks.toml`.

Groups:

- `quality`
- `test`
- `release-verify`
- `site`
- `pr`
- `full`

Key tasks:

- `ci-self-check`: `cargo run -q -p vo-dev -- lint all`
- `d-py-smoke`: `./d.py help`
- `eng-lint-release`: `cargo run -q -p vo-dev -- lint release`
- `cargo-fmt`: `cargo fmt --all -- --check`
- `cargo-clippy`: `cargo clippy --workspace --all-targets --exclude vo-playground -- -D warnings`
- `cargo-check`: `cargo check --workspace --all-targets --exclude vo-playground`
- `wasm-check`: `cargo check -p vo-web --target wasm32-unknown-unknown`
- `vo-test`: `cargo run -q -p vo-dev -- test run --suite lang --targets vm,jit --release`
- `vo-test-compile`: `cargo run -q -p vo-dev -- test run --suite lang --targets compile --release`
- `vo-test-gc`: `cargo run -q -p vo-dev -- test run --suite lang --targets gc-vm,gc-jit --release`
- `vo-test-osr`: `cargo run -q -p vo-dev -- test run --suite lang --targets osr --release`
- `vo-test-nostd`: `cargo run -q -p vo-dev -- test run --suite lang --targets nostd --release`
- `vo-test-wasm`: `cargo run -q -p vo-dev -- test run --suite lang --targets wasm`
- `cargo-test-release`: `cargo test --workspace --release --exclude vo-playground`
- `wasm-check-release`: release WASM check
- site tasks: vo-web wasm build, vogui build, Studio WASM/build, quickplay validation, static BlockKart smoke, and deploy-time remote BlockKart smoke through `BLOCKKART_SMOKE_BASE_URL`
- release verification for sibling first-party repos

Planner/runner:

- `cargo run -q -p vo-dev -- task plan <selector>`: selects tasks by group, exact task, or changed-file triggers.
- `cargo run -q -p vo-dev -- task run <selector>`: executes groups and individual tasks.
- `cargo run -q -p vo-dev -- ci metadata <selector> --github-output`: emits union tool versions, Node lockfiles, and a single first-party checkout for workflow jobs that run a whole group in one job, such as Pages deployment.
- `cargo run -q -p vo-dev -- verify plan <selector>`: prints selected tasks plus tool, cwd, command-path, and declared repo readiness before execution.
- `cargo run -q -p vo-dev -- verify run <selector>`: checks tools, then runs the selected task set.
- GitHub Actions and `./d.py ci ...` call `vo-dev` directly.
- Deploy workflows should run task graph selectors rather than hand-maintaining `paths` filters for task inputs.
- Changed-file planning adds both prerequisites and downstream dependents declared through `needs`, so selecting a generator/check task can also select its consumer.

Native language-test CI tasks use `--release` to run the `vo-test` executor and
embed prebuilds with optimized Rust code. This intentionally amortizes native
language-test compilation with `cargo-test-release`. Focused development
commands may omit `--release` when fast debug rebuilds are more useful than
full-suite throughput. `vo-test-wasm` stays on the normal WASM test build path;
release WASM coverage is owned by `wasm-check-release`.

Release workflow:

- `cargo run -q -p vo-dev -- release matrix --github-output`: emits the GitHub build matrix from `eng/release.toml`.
- `cargo run -q -p vo-dev -- release cross-version`: prints the pinned `cross` crate version.
- `cargo run -q -p vo-dev -- release homebrew-metadata --github-output`: emits the Homebrew tap repository, checkout path, and formula path from `eng/release.toml`.
- `cargo run -q -p vo-dev -- release build <target>`: adds the Rust target and runs Cargo or cross with the release package policy.
- `cargo run -q -p vo-dev -- release package <target>`: writes `vo-<target>.tar.gz` and its `.sha256`.
- `cargo run -q -p vo-dev -- release notes|publish|update-homebrew ...`: owns release notes, GitHub Release publication, and Homebrew formula mutation. `release publish` accepts only the exact tarball/checksum set declared by `eng/release.toml`; publication and Homebrew updates reject malformed or mismatched checksum files, and Homebrew formula targets must match the release target set. The workflow should only do checkout, setup, artifact transfer, and final git push.
- `cargo run -q -p vo-dev -- lint release`: validates release slug/path shapes, `build_args` consistency with the declared crate, rejects target-specific build args, checks release profile values, target uniqueness, and Homebrew metadata.

Git hooks:

- pre-commit: `cargo fmt --all -- --check`
- pre-push: default `./d.py ci quality`, configurable via `VO_LOCAL_CI_MODE`

## Benchmarks

Benchmarks live under `benchmarks/<name>` with Vo and multi-language implementations.

`./d.py bench` can run:

- all benchmarks
- only Vo
- a named benchmark
- score generation
- optional all-language comparisons

The benchmark runner builds release `vo`, can use `hyperfine`, and writes generated results under `benchmarks/results`. It can also create language-specific build artifacts such as `go_bench`, `c_bench`, `.class`, etc.

Do not treat benchmark directories as clean source-only areas.

## Validation Selection

Use the narrowest command that covers the risk.

Syntax/parser-only:

- `cargo test -p vo-syntax`
- focused `./d.py test both <file>` if executable behavior is affected
- `cargo fmt --all -- --check`

Type checker/codegen/runtime:

- `./d.py test both <file>`
- `./d.py test both`
- `./d.py test jit <file>` for JIT-sensitive changes
- `./d.py test gc` for GC-sensitive changes

No_std/embed:

- `./d.py test nostd` or `./d.py test embed`
- inspect `cmd/vo-embed`

WASM/browser compiler:

- `cargo check -p vo-web --target wasm32-unknown-unknown`
- `./d.py test wasm`

CLI/module:

- `cargo build -p vo`
- targeted `./d.py vo mod ...` in a temp project if needed
- module lifecycle tests

Stdlib:

- `./d.py test both <stdlib-related-test>`
- `./d.py test wasm` if browser/no_std exposed
- `cargo check --workspace --all-targets --exclude vo-playground`

Studio:

- `./d.py ci task studio-build` as the focused task-graph frontend check
- `./d.py ci site` for the deploy-equivalent path that also covers local `vogui` installation
- `cd apps/studio && npm run build` only for local Vite/Svelte debugging, not final repo verification
- `./d.py studio --build-only`
- `./d.py studio` for interactive local verification

CI confidence:

- `cargo run -q -p vo-dev -- verify plan pr`
- `cargo run -q -p vo-dev -- verify run quality`
- `./d.py ci smart`
- `./d.py ci quality`
- `./d.py ci test`
- `./d.py ci pr`

## Environment Notes

- Rust toolchain is pinned in `rust-toolchain.toml` and includes rustfmt, clippy, and `wasm32-unknown-unknown`.
- Tool requirements are declared in `eng/toolchains.toml`.
- Node >=24.0 is required for Studio/Playground/site tasks. CI uses Node 24 as the minimum supported baseline.
- `wasm-pack` 0.14.0 is required for WASM package builds.
- Use `cargo run -q -p vo-dev -- tool bootstrap --task <task>` to print the exact local setup actions for a task.
- GitHub module downloads use `VO_GITHUB_TOKEN` or `GITHUB_TOKEN` if available.
- Module cache defaults to `$HOME/.vo/mod`.
- `vo.work` may redirect imports to sibling repos. Use `VOWORK=off` for hermetic language tests.
- Studio/site tasks may depend on sibling first-party repos such as `vogui`; first-party hints live in `eng/project.toml` and can be overridden with `CI_MODULE_ROOT`.

## Caveats

- Broad and focused language test selection is manifest-driven through `vo-dev`; `./d.py test ... <file>` is a compatibility wrapper around `vo-dev test run --path`.
- `vo test` is not the same as `./d.py test both`.
- Some commands can generate artifacts in benchmarks, wasm package directories, or Studio public/dist directories.
- Network-dependent module lifecycle behavior should be tested explicitly, not by relying on frozen build commands.
