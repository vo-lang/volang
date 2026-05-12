# Volang Repository Map

## Contents

- [Repo Shape](#repo-shape)
- [Crate Responsibilities](#crate-responsibilities)
- [Command And Tool Entrypoints](#command-and-tool-entrypoints)
- [End-To-End Compile And Run](#end-to-end-compile-and-run)
- [Docs And Generated Content](#docs-and-generated-content)
- [Source Truth Caveats](#source-truth-caveats)

## Repo Shape

Volang is a Rust workspace with Vo source, Svelte frontends, WASM bindings, module tooling, and test data in one repo.

- `Cargo.toml`: workspace root. Members include `lang/crates/*`, `cmd/vo`, `cmd/vo-embed`, `cmd/vo-test`, `apps/playground-legacy/rust`, and `lang/crates/vo-web/runtime-wasm`.
- `cmd/vo`: main CLI.
- `cmd/vo-dev`: engineering control plane for tasks, CI planning, tool checks, test manifests, artifact policy, and repository boundary lints.
- `cmd/vo-test`: native `run-plan` language test execution engine.
- `cmd/vo-embed`: no_std-oriented bytecode runner.
- `lang/crates`: compiler, runtime, module system, stdlib, web, and app runtime crates.
- `lang/stdlib`: Vo facade source for stdlib packages.
- `tests/lang`: real language regression/integration tests.
- `lang/docs/spec`: current spec background docs. Use source as final authority.
- `lang/docs/dev-notes`: design notes and historical plans. Useful for intent, often stale.
- `lang/docs/dev-notes/2026-05-11-engineering-and-repository-redesign.md`: canonical engineering-control-plane and repository-layout redesign.
- `apps/studio`: current Svelte Studio app, Studio WASM crate, and Tauri shell.
- `apps/playground-legacy`: legacy Svelte Playground app and its WASM bridge.
- `benchmarks`: Vo and multi-language benchmark programs and generated benchmark artifacts.
- `eng`: canonical task graph, toolchain requirements, CI planning policy, artifact policy, first-party repo data, release policy, and test target definitions. Start with `eng/README.md`.
- `tests/lang/manifest.toml`: canonical language regression test manifest.
- `scripts/ci`: narrow quickplay helper scripts. Self-check, task planning, task execution, first-party repo lookup, and release verification live in `cmd/vo-dev`.
- `vo.work`: sibling repository overrides for local first-party development.

## Crate Responsibilities

Use this table for first-pass routing.

| Path | Role |
| --- | --- |
| `lang/crates/vo-common` | Diagnostics, source map, span, symbol, stable hash, VFS abstractions, ABI name helpers. |
| `lang/crates/vo-common-core` | Bytecode module model, instruction format, runtime type metadata, serialization, debug info, log records. |
| `lang/crates/vo-syntax` | Lexer, parser, AST, inline module syntax extraction, source formatter. |
| `lang/crates/vo-analysis` | Project analysis, imports, type checker, scopes, objects, packages, selections, type info. |
| `lang/crates/vo-codegen` | Type-checked project to `Module` bytecode, type/runtime metadata, wrappers, itabs, init/entry functions. |
| `lang/crates/vo-engine` | Public compile/run/check/format API, module context integration, dependency readiness, native extension preparation. |
| `lang/crates/vo-vm` | VM, interpreter exec, scheduler, fibers, GC root scanning, JIT integration points. |
| `lang/crates/vo-runtime` | Shared runtime object model, GC heap, FFI registry/loader, builtins, islands, output, queues/ports. |
| `lang/crates/vo-jit` | Cranelift JIT compiler and loop OSR support. |
| `lang/crates/vo-module` | Module identity, manifests, lock files, workspace overrides, solver, registry, cache, lifecycle operations. |
| `lang/crates/vo-stdlib` | Embedded stdlib source and Rust native implementations for stdlib externs. |
| `lang/crates/vo-ffi-macro` | `#[vo_fn]` and `#[vostd_fn]` proc macros. |
| `lang/crates/vo-ext` | Native extension SDK and exported C ABI table. |
| `lang/crates/vo-web` | WASM bindings for compile/run, browser VFS, registry, runtime plans, extension bridge. |
| `lang/crates/vo-app-runtime` | Shared app/session runtime for GUI hosts, host events, render buffers, native event loop. |
| `lang/crates/vo-release` | Release verification and staging support. |

## Command And Tool Entrypoints

- Main CLI dispatch: `cmd/vo/src/main.rs`.
- `vo run`: `cmd_run`, then `vo_engine::compile_with_auto_install`, then `vo_engine::run`.
- `vo build` / `vo emit`: compile and serialize bytecode with `Module::serialize`.
- `vo check`: `vo_engine::check_with_auto_install`.
- `vo fmt`: `vo_engine::format_source`, parser plus `vo_syntax::display`.
- `vo dump`: bytecode deserialize plus `vo_engine::format_text`.
- `vo mod ...`: CLI wrappers around `vo_module::ops::*`.
- `./d.py`: repo developer wrapper. Must run from repo root.
- `cmd/vo-dev`: implementation for bench, loc, clean, Studio, WASM, CI planning, first-party repo/workspace helpers, and task execution helpers. `main.rs` is only top-level dispatch. `dev_bench.rs` owns `vo-dev bench`, `dev_clean.rs` owns `vo-dev clean`, `dev_loc.rs` owns `vo-dev loc`, `dev_studio.rs` owns Studio launch/stop and WASM freshness helpers, and `dev_common.rs` owns shared developer-tool constants/helpers. `task_system.rs` owns `vo-dev task` dispatch and output shaping, `task_graph.rs` owns task maps/group resolution/dependency expansion/tool and Node workspace collection, `task_planner.rs` owns selector and changed-file planning plus Git line helpers, and `task_runner.rs` owns task execution/output validation/tool readiness/timeouts. `ci_system.rs` owns `vo-dev ci`, matrix/metadata output, and CI checkout/cache shaping. `verify_system.rs` owns `vo-dev verify` readiness reports. `test_config.rs` owns `eng/tests.toml` target/alias parsing and selector expansion. `test_manifest.rs` owns `tests/lang/manifest.toml` parsing, path materialization, and manifest lint policy. `test_system.rs` owns `vo-dev test` dispatch and output shaping, `test_plan.rs` owns language-test planning/path filtering/compile-fail expansion, and `test_runner.rs` owns native/WASM execution dispatch. `release_config.rs` owns release config validation and artifact/checksum policy; `release_homebrew.rs` owns Homebrew formula target/version/SHA helpers; `release_system.rs` owns release command dispatch. `artifact_lint.rs` owns artifact schema/provenance/task coverage/size policy, `artifact_repo_lint.rs` owns tracked/ignored generated artifact repository scans, `command_lint.rs` owns task command tool inference, first-party nested command checks, and `vo-dev test run` task-tool inference. `lint_system.rs` owns lint dispatch plus task/repo-boundary lint execution, `lint_policy.rs` owns shared low-level lint path/reference/slug policy, `tool_lint.rs` owns toolchain, Node workspace, and Rust cache workspace lint policy, `tool_system.rs` owns tool checks/bootstrap/version resolution, `github_output.rs` owns GitHub output-file formatting, and `first_party.rs` owns sibling repo/workspace lookup.
- `cmd/vo-dev`: `task`, `ci`, `tool`, `verify`, `test`, `lint`, `release`, `bench`, `loc`, `clean`, and Studio subcommands.
- `eng/tasks.toml`: local and GitHub CI task graph.

## End-To-End Compile And Run

The native compile/run pipeline is:

1. CLI or library calls `vo_engine::compile*` / `check*`.
2. `lang/crates/vo-engine/src/compile/mod.rs` chooses real path, memory, cache, or auto-install path.
3. `lang/crates/vo-engine/src/compile/pipeline.rs` builds a project context, checks frozen dependency readiness, collects a `FileSet`, resolves stdlib and modules, and calls analysis.
4. `vo_analysis::analyze_project*` parses files, preloads imports, type-checks packages, records type info, and discovers extension manifests.
5. `vo_codegen::compile_project` registers runtime metadata, collects declarations, compiles functions, builds init/entry wrappers, finalizes itabs/runtime types, and returns a `Module`.
6. `lang/crates/vo-engine/src/run.rs` loads native extension specs, creates `Vm::new` or `Vm::with_jit_config`, registers output and program args, calls `vm.load_with_extensions`, and runs the scheduler.
7. `vo-vm` executes bytecode via the interpreter, with optional JIT side paths under the `jit` feature.

## Docs And Generated Content

Prefer these current docs:

- Language: `lang/docs/spec/language.md`
- Module system: `lang/docs/spec/module.md`
- Inline module tutorial: `lang/docs/spec/module-inline-mod-tutorial.md`
- Native FFI: `lang/docs/spec/native-ffi.md`, but verify macro names against source.
- Bytecode/JIT/memory specs: useful background, but verify opcodes and ABI against source.
- Go migration: `lang/docs/vo-for-gophers.md`

Common generated/vendor paths:

- `target/`
- `apps/studio/dist/`
- `apps/studio/node_modules/`
- `apps/studio/public/wasm/` and `apps/studio/dist/wasm/`
- `apps/studio/public/quickplay/blockkart/`: checked-in generated quickplay package; artifact policy and provenance are in `eng/artifacts.toml`.
- `lang/crates/vo-web/pkg/`
- `benchmarks/results/`
- benchmark build products such as `go_bench`, `c_bench`, `.class`

## Source Truth Caveats

- `lang/crates/vo-syntax/README.md` contains old parser API examples.
- Playground generated docs must come from `lang/docs/spec` through `scripts/ci/docs_sync.mjs`.
- Some dev notes describe intended future architecture rather than current behavior.
- Use `lang/docs/dev-notes/2026-05-11-engineering-and-repository-redesign.md` for the canonical engineering-control-plane and repository-layout target design.
- Bytecode spec names can lag source. Current opcode names and encodings live in `lang/crates/vo-common-core/src/instruction.rs` and generated/VM call sites.
- JIT docs are directional. Actual support is per opcode and may fall back to the VM.
- Build-like commands do not re-resolve dependencies or rewrite module files, but current auto-install paths can download artifacts already pinned by `vo.lock`. Explicit `vo mod` commands own dependency resolution and lock mutation.
