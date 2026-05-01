# Volang Agent Guide

This file is the working guide for AI agents and automation touching this
repository. It summarizes the current project shape, the most important local
commands, and the invariants that should guide code and documentation changes.

## Mental Model

Volang is both a language implementation and a development environment.

- The language pipeline lives under `lang/`: syntax, analysis, codegen, VM,
  runtime, JIT, stdlib, module tooling, release tooling, and web runtime.
- The command line tools live under `cmd/`, especially `cmd/vo` and
  `cmd/vo-test`.
- Vo Studio lives under `studio/`: Svelte web app, Rust/WASM compiler bridge,
  and Tauri native wrapper.
- Local and CI workflows are routed through `d.py` and `scripts/ci/`.
- First-party modules such as `vogui`, `voplay`, `vopack`, and `vostore` are
  usually sibling repositories and may be referenced through `vo.work`.

Prefer current canonical formats and rules. If old metadata, package formats,
or docs conflict with `lang/docs/spec/`, update toward the current spec instead
of adding long-lived compatibility paths unless a migration design explicitly
requires that.

## Repository Map

```text
volang/
|-- cmd/
|   |-- vo/                 # main CLI
|   |-- vo-test/            # language/runtime integration test runner
|   `-- vo-embed/           # no_std/embed-oriented runner
|-- lang/
|   |-- crates/             # Rust crates for compiler/runtime/tooling
|   |-- stdlib/             # Vo standard library source packages
|   |-- test_data/          # integration tests and fixture projects
|   |-- tools/              # language-side helper tooling
|   `-- docs/
|       |-- spec/           # canonical specs
|       |-- dev/            # stable developer docs
|       `-- dev-notes/      # dated investigation/onboarding notes
|-- studio/
|   |-- src/                # Svelte web app
|   |-- wasm/               # wasm-bindgen bridge to compiler/runtime
|   |-- src-tauri/          # native Studio backend
|   |-- docs/               # user-facing docs rendered in Studio
|   |-- public/             # served static assets, wasm, quickplay packages
|   `-- scripts/            # Studio build/package helpers
|-- scripts/ci/             # CI task graph and local runner
|-- examples/               # sample Vo programs
|-- benchmarks/             # benchmark suite
|-- playground/             # older playground app code
|-- d.py                    # stable dev command wrapper
|-- d_py.py                 # heavier command implementation
|-- Cargo.toml              # Rust workspace
`-- vo.work                 # local first-party module workspace overrides
```

## Rust Crate Routing

Use this as a first-pass map when choosing where to work:

- `vo-syntax`: parser and syntax tree structures.
- `vo-analysis`: type checking, object model, selection info, slot layout.
- `vo-codegen`: lowers checked programs into bytecode and slot metadata.
- `vo-common` and `vo-common-core`: shared diagnostics, bytecode definitions,
  runtime type metadata.
- `vo-runtime`: GC, object layouts, builtins, queues, slices, maps, strings,
  island messages.
- `vo-vm`: bytecode interpreter, fibers, scheduler, extern calls, GC root
  scanning, transport.
- `vo-jit`: Cranelift JIT, loop OSR, JIT helpers.
- `vo-stdlib`: Rust-backed standard library externs.
- `vo-module`: module resolution, lock files, cache/install logic.
- `vo-release`: module release verification and staging.
- `vo-ext` and `vo-ffi-macro`: native/WASM extension boundary.
- `vo-engine`: high-level compile/run API used by CLI and embedding.
- `vo-web`: browser/WASM runtime surface.
- `vo-app-runtime`: app/session runtime glue, especially GUI/island execution.

## Command Entry Point

Run `./d.py` from the repository root. The wrapper intentionally exits when
invoked from another directory.

Common commands:

```sh
./d.py test both
./d.py test vm
./d.py test jit
./d.py test jit --release
./d.py test gc
./d.py test nostd
./d.py test wasm
./d.py test both path/to/test.vo

./d.py vo check path/to/project
./d.py vo build path/to/project -o target/out.vob
./d.py vo dump target/out.vob
./d.py run path/to/file.vo --mode=vm
./d.py run path/to/file.vo --mode=jit

./d.py studio
./d.py studio --build-wasm
./d.py studio --build-only
./d.py studio --runner <project-or-url>
./d.py studio-native
./d.py studio-stop

./d.py ci smart
./d.py ci quality
./d.py ci test
./d.py ci site
./d.py ci pr
./d.py ci full
./d.py ci task <task-name>
```

Notes:

- `./d.py test` runs the large `lang/test_data/` integration suite. It is the
  main language-core regression harness and can cover thousands of test cases.
  Run it when changing parser, analysis, codegen, VM, runtime, JIT, stdlib, or
  closely related CLI behavior.
- JIT-mode tests are much slower in debug builds. Prefer
  `./d.py test jit --release` for broad JIT verification.
- `./d.py test wasm` uses the Node/WASM path through `d_py.py`.
- `./d.py studio` rebuilds Studio WASM when stale, then starts Vite.
- Studio web uses strict Vite port `5174`.
- `./d.py studio-native` launches Tauri and can reuse an existing Studio dev
  server.
- `./d.py studio-stop` only stops the matching Studio Vite process.
- `--release` selects release Rust binaries for wrapper-dispatched CLI
  commands; `d.py` accepts it anywhere in the argument list.

## Development Workflows

### Language Semantics

Start in:

```text
lang/crates/vo-analysis
lang/crates/vo-codegen
lang/crates/vo-vm
lang/crates/vo-runtime
lang/stdlib
lang/test_data
```

Typical verification:

```sh
cargo check --workspace --all-targets --exclude vo-playground
./d.py test both
./d.py test jit --release
./d.py test gc
cargo check -p vo-web --target wasm32-unknown-unknown
```

Add behavior tests under `lang/test_data/`. Put Rust unit tests near the
compiler/runtime code they protect.

### CLI and Tooling

Start in:

```text
cmd/vo
cmd/vo-test
d.py
d_py.py
lang/crates/vo-engine
lang/crates/vo-module
lang/crates/vo-release
```

Typical verification:

```sh
./d.py vo check path/to/project
./d.py test both
./d.py ci smart
```

### Module and Release Logic

Start in:

```text
lang/crates/vo-module
lang/crates/vo-release
lang/docs/spec/module.md
lang/docs/spec/repository-layout.md
lang/docs/spec/native-ffi.md
scripts/ci/tasks.toml
```

Typical verification:

```sh
./d.py vo mod sync path/to/module
./d.py vo mod verify path/to/module
./d.py vo mod download path/to/module
./d.py vo release verify path/to/module
./d.py ci release-verify
```

### Studio Web UI

Start in:

```text
studio/src/App.svelte
studio/src/components
studio/src/lib/router.ts
studio/src/lib/services
studio/src/lib/backend
studio/src/stores
```

Typical verification:

```sh
cd studio
npm run build
```

For live development, run from repo root:

```sh
./d.py studio
```

### Studio WASM or Compiler Used by Studio

Start in:

```text
studio/wasm/src/lib.rs
studio/src/lib/studio_wasm.ts
lang/crates/*
lang/stdlib
```

Typical verification:

```sh
./d.py studio --build-only
cargo check -p vo-web --target wasm32-unknown-unknown
```

If Studio reports an asset mismatch, rebuild Studio WASM and reload the page.
The local/deploy build id is in `studio/public/wasm/vo_studio_wasm.build_id`.

### Native Studio

Start in:

```text
studio/src-tauri/src/main.rs
studio/src-tauri/src/lib.rs
studio/src-tauri/src/commands
studio/src-tauri/src/gui_runtime.rs
studio/src/lib/backend/native_backend.ts
```

Native Studio exposes direct filesystem and process capabilities. Keep shared
UI code backend-neutral through `studio/src/lib/backend/backend.ts` where
possible.

### Studio Docs

User-facing docs rendered by Studio live in `studio/docs/`.

```text
studio/docs/getting-started/
studio/docs/language/
studio/docs/cli/
studio/docs/advanced/
```

When adding user-facing language or tool docs, prefer `studio/docs/`. When
writing engineering notes or handoffs, use `lang/docs/dev/` or
`lang/docs/dev-notes/`.

## Module System Invariants

The canonical module spec is `lang/docs/spec/module.md`, with repository layout
rules in `lang/docs/spec/repository-layout.md`.

Important rules for agents:

- Published module paths are canonical GitHub paths:
  `github.com/<owner>/<repo>[/<subdir>][/vN]`.
- Non-stdlib imports must use full canonical paths beginning with
  `github.com/`; relative imports are invalid.
- Standard library imports are bare paths such as `fmt`, `strings`, and
  `encoding/json`.
- `vo.mod` records authored intent. `vo.lock` records the exact resolved graph.
- The root project's `vo.lock` is authoritative for builds.
- `vo build`, `vo check`, `vo test`, and `vo run` are frozen-build operations:
  they must not access the network, mutate `vo.mod`, mutate `vo.lock`, or
  re-solve dependencies.
- `vo.work` may replace module source locally, but must not change canonical
  module identity or published lockfile semantics.
- Integrity mismatches should fail hard, not fall back to unchecked behavior.

For module repository layout, commit `vo.mod` and `vo.lock`; do not commit
`.vodeps/`, `.vo-cache/`, `target/`, or native dynamic library outputs.

## Studio Architecture Notes

Studio has three main layers:

- Web app: Svelte UI, routing, stores, editor, docs, runner, and backend-neutral
  services.
- Studio WASM: wasm-bindgen exports in `studio/wasm/src/lib.rs`, loaded through
  `studio/src/lib/studio_wasm.ts` from static files in `studio/public/wasm/`.
- Native Studio: Tauri commands and native runtime bridge under
  `studio/src-tauri/`.

UI modes are:

- `manage`: home and project management.
- `develop`: editor/workbench mode.
- `docs`: rendered Markdown docs.
- `runner`: lightweight run-only mode for share/play links.

Launch URLs use query parameters such as `mode` and `proj`; the hash represents
visible mode routing such as `#/develop`, `#/runner`, or `#/docs/...`.

## Quickplay and Local Projects

Quickplay packages are the static, run-fast path for curated Studio projects.
Relevant files include:

```text
studio/src/lib/quickplay.ts
studio/public/quickplay/
studio/scripts/
scripts/ci/quickplay_validate.mjs
```

The usual pattern is:

1. Package project source into a static `project.json`.
2. Package dependency source/artifacts into `deps.json` plus static artifact
   files.
3. Serve those files from `studio/public/quickplay/...`.
4. Open the package through a stable `vo:quickplay:<name>` spec.

In local web development, Studio can snapshot readable local projects through
Vite middleware in `studio/vite.config.ts`. Relevant environment variables are:

```text
VITE_STUDIO_LOCAL_PROJECTS=1
VITE_STUDIO_LOCAL_PROJECT_MAX_BYTES
VITE_STUDIO_LOCAL_PROJECT_MAX_FILES
```

The snapshot path should include source and declared artifacts, but skip large
or generated directories such as `.git`, `node_modules`, and `target`.

## CI and Verification

The local CI task graph is in:

```text
scripts/ci/tasks.toml
scripts/ci/run.py
scripts/ci/plan.py
```

Common groups:

- `smart`: changed-file based default selection.
- `quality`: format, clippy, cargo check, WASM check.
- `test`: language tests and release-mode cargo/WASM checks.
- `site`: web/Studio/quickplay build and smoke checks.
- `release-verify`: first-party module release validation.
- `pr`: broad pull-request gate.
- `full`: everything.

Prefer the smallest verification that covers the touched surface, then widen if
the change crosses subsystem boundaries.

## Documentation Source of Truth

Read these first when the task touches the corresponding area:

```text
README.md
lang/docs/spec/language.md
lang/docs/spec/module.md
lang/docs/spec/repository-layout.md
lang/docs/spec/native-ffi.md
lang/docs/spec/vm-bytecode.md
lang/docs/spec/vm-jit-design.md
lang/docs/dev/d-py-usage.md
lang/docs/dev-notes/
studio/docs/_manifest.json
studio/docs/cli/commands.md
studio/docs/advanced/backends.md
studio/docs/advanced/modules.md
studio/docs/advanced/embedding.md
```

Treat `lang/docs/spec/` as the stronger source for canonical behavior.
Treat `studio/docs/` as the stronger source for user-facing Studio docs.
Treat dated notes in `lang/docs/dev-notes/` as useful history and handoff
context, not automatically-current specs.

## Agent Working Rules

- Use `rg` first for code and text search.
- Keep changes scoped to the subsystem implied by the task.
- Preserve user changes already present in the worktree.
- Prefer existing local patterns and helper APIs over new abstractions.
- Add abstractions only when they remove real complexity or match an existing
  pattern.
- Use structured parsers or existing APIs for structured data instead of
  hand-rolled string manipulation.
- Do not write overly defensive code that hides invalid states. Prefer
  fail-fast behavior with clear errors over broad fallbacks or silent recovery.
- For frontend changes, match the existing Svelte/Studio design language and
  verify that the app builds.
- For language/runtime changes, add focused `lang/test_data/` coverage when
  behavior changes.
- For module/release changes, keep `vo.mod`, `vo.lock`, `vo.work`,
  `vo.release.json`, and `vo.web.json` semantics aligned with the specs.
- When a change would break an existing format or compatibility contract, ask a
  human to choose whether to fully clean up the old format or preserve
  compatibility.
- Do not broaden compatibility with obsolete formats unless the task explicitly
  asks for a migration path.
