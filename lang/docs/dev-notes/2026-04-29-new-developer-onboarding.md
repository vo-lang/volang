# Volang Developer Onboarding

Date: 2026-04-29

This note is a compact entry map for new contributors. It covers the main repo, Vo Studio, local tools, CI, module/release flow, and related first-party projects. It is intentionally general; active issue investigations should be documented in separate issue-specific notes.

## Mental Model

Volang is both a language implementation and a browser/native development environment.

- The language pipeline lives under `lang/`: parser, analysis, codegen, VM, runtime, JIT, stdlib, module system, release tooling, and web runtime.
- The CLI lives under `cmd/vo`.
- The test runner lives under `cmd/vo-test`.
- Studio lives under `studio/`: Svelte web app, Rust/WASM compiler bridge, and Tauri native wrapper.
- CI lives mostly in `scripts/ci/` and `.github/workflows/`.
- First-party libraries such as `vogui`, `voplay`, `vopack`, and `vostore` are sibling repos and are validated by Volang release checks.

The repo prefers current canonical formats and rules. If old metadata, docs, or package formats are found, update them to the latest shape instead of adding long-lived compatibility paths unless a migration design explicitly requires it.

## Top-Level Map

```text
volang/
├── cmd/
│   ├── vo/              # CLI executable
│   ├── vo-test/         # integration test runner
│   └── vo-embed/        # no_std/embed-oriented runner
├── lang/
│   ├── crates/          # Rust crates for compiler/runtime/tooling
│   ├── stdlib/          # Vo standard library sources
│   ├── test_data/       # language/runtime integration tests
│   └── docs/            # specs, dev docs, dev-notes
├── studio/
│   ├── src/             # Svelte web app
│   ├── wasm/            # wasm-bindgen bridge to compiler/runtime
│   ├── src-tauri/       # native Studio backend
│   ├── docs/            # public Studio docs shown in the app
│   ├── public/          # served static assets, wasm, quickplay packages
│   └── scripts/         # Studio build/package helpers
├── scripts/ci/          # task graph, local CI runner, CI helpers
├── examples/            # sample Vo programs
├── benchmarks/          # benchmark suite
├── playground/          # older playground-related app code
├── d.py                 # main dev command wrapper
└── d_py.py              # implementation for heavier dev commands
```

## Core Crates

Use this as a first-pass routing table:

- `vo-syntax`: syntax tree/parser-level structures.
- `vo-analysis`: type checking, object model, selection info, slot layout helpers.
- `vo-codegen`: lowers checked programs into bytecode and slot metadata.
- `vo-common` and `vo-common-core`: shared diagnostics, bytecode definitions, runtime type metadata.
- `vo-runtime`: GC, object layouts, builtins, queues, slices, maps, strings, island messages.
- `vo-vm`: bytecode interpreter, fibers, scheduler, extern calls, GC root scanning, transport.
- `vo-jit`: Cranelift JIT, loop OSR, JIT helpers.
- `vo-stdlib`: Rust-backed stdlib externs.
- `vo-module`: module resolution, lock files, cache/install logic.
- `vo-release`: release verification/staging for Vo modules.
- `vo-ext` and `vo-ffi-macro`: native/WASM extension boundary.
- `vo-engine`: high-level compile/run API used by CLI and embedding.
- `vo-web`: browser/WASM runtime surface.
- `vo-app-runtime`: app/session runtime glue, especially GUI/island execution.

## `d.py`

Always run `./d.py` from the repo root. It checks the current directory and exits if invoked elsewhere.

`d.py` is the stable entry point. It directly handles common CLI/test dispatch and delegates heavier workflows to `d_py.py`.

Common commands:

```sh
./d.py test both
./d.py test vm
./d.py test jit
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

- `./d.py test wasm` goes through the Node/WASM path.
- `./d.py studio` rebuilds Studio WASM when it is stale, then starts Vite.
- Studio web uses strict Vite port `5174`.
- `./d.py studio-native` launches Tauri and can reuse an existing Studio dev server.
- `./d.py studio-stop` only stops a matching Studio Vite process on the fixed port.
- `--release` before the command selects release Rust binaries for the wrapper path, for example `./d.py --release vo version`.

## Studio Architecture

Studio has three major layers.

### Web App

Important files:

- `studio/src/App.svelte`: top-level boot, mode binding, session opening, runner auto-run.
- `studio/src/lib/router.ts`: hash/search URL routing for manage/develop/docs/runner.
- `studio/src/stores/`: Svelte stores for IDE mode, editor, session, runtime, console.
- `studio/src/components/`: UI surfaces: home, dev workbench, runner, docs, editor, file tree, console.
- `studio/src/lib/services/`: service layer that sits between UI components and backend implementations.
- `studio/src/lib/backend/backend.ts`: backend capability interface shared by web and native.
- `studio/src/lib/backend/web_backend.ts`: browser/WASM backend implementation.
- `studio/src/lib/backend/native_backend.ts`: Tauri backend implementation.

UI modes:

- `manage`: home/project management.
- `develop`: editor/workbench mode.
- `docs`: rendered markdown docs.
- `runner`: lightweight run-only mode for share/play links.

Launch URLs use query parameters such as `mode` and `proj`, while the hash represents user-visible mode routing (`#/develop`, `#/runner`, `#/docs/...`).

### Studio WASM

Important files:

- `studio/wasm/src/lib.rs`: wasm-bindgen exports for compile/check/run GUI, VFS, extension preload, island transport, docs/runtime helpers.
- `studio/src/lib/studio_wasm.ts`: TypeScript loader and typed surface for `vo_studio_wasm`.
- `studio/scripts/build_wasm.mjs`: builds and stamps the Studio WASM package.
- `studio/public/wasm/vo_studio_wasm.build_id`: local/deploy build id used to prevent stale JS/WASM mismatches.

The web app does not bundle the Studio WASM module. It loads `/wasm/vo_studio_wasm.js` and `/wasm/vo_studio_wasm_bg.wasm` from `studio/public/wasm/`, with a build-id query parameter.

If the browser reports an asset mismatch, rebuild Studio WASM:

```sh
./d.py studio --build-only
```

Then reload the dev page.

### Native Studio

Important files:

- `studio/src-tauri/src/main.rs` and `lib.rs`: Tauri entry/wiring.
- `studio/src-tauri/src/commands/`: native commands for compiler, workspace, session, GUI, git, HTTP, process, dialogs.
- `studio/src-tauri/src/gui_runtime.rs`: native GUI runtime bridge.

Native Studio exposes more direct filesystem/process capabilities than the web backend. The shared TypeScript backend interface keeps most UI code backend-neutral.

## Studio Docs

Public documentation used by Studio is under `studio/docs/`:

- `getting-started/`: introduction, installation, hello world.
- `cli/commands.md`: CLI reference.
- `language/`: syntax, error handling, dynamic access, Go differences.
- `advanced/`: backends, embedding, modules.

When adding user-facing language or tool docs, prefer `studio/docs/`. When writing engineering notes, use `lang/docs/dev/` or `lang/docs/dev-notes/`.

## Local Web Project Snapshots

In web dev mode, Studio can open local projects by snapshotting readable files through a Vite middleware in `studio/vite.config.ts`.

Relevant env vars:

- `VITE_STUDIO_LOCAL_PROJECTS=1`
- `VITE_STUDIO_LOCAL_PROJECT_MAX_BYTES`
- `VITE_STUDIO_LOCAL_PROJECT_MAX_FILES`

The snapshot logic includes `.vo`, `vo.mod`, `vo.lock`, `vo.work`, package artifacts, selected assets, and common web/WASM artifact paths. It skips large/generated directories such as `.git`, `node_modules`, and `target`.

## Quickplay Packages

Quickplay is the static run-fast path for curated projects. Current implementation details are in:

- `studio/src/lib/quickplay.ts`
- `studio/public/quickplay/`
- `studio/scripts/`
- `scripts/ci/quickplay_validate.mjs`
- quickplay smoke checks under `scripts/ci/`

The pattern is:

1. Package project source into a static `project.json`.
2. Package dependency source/artifacts into `deps.json` plus static artifact files.
3. Serve those files from `studio/public/quickplay/...`.
4. Let Studio open the package through a stable `vo:quickplay:<name>` spec.

The package script uses the module lock/cache state, not arbitrary local sibling worktrees, unless explicitly configured by environment.

## Modules, Workspaces, and First-Party Libraries

Vo modules use:

- `vo.mod`: module metadata, dependencies, extension metadata.
- `vo.lock`: resolved dependency/artifact lock.
- `vo.work`: local workspace overrides for development.

The root `vo.work` points to sibling first-party modules such as `vogui`, `voplay`, and `vopack`. That is convenient for local development, but release/quickplay verification should be clear about whether it is using workspace overrides or locked artifacts.

Useful commands:

```sh
./d.py vo mod sync path/to/module
./d.py vo mod verify path/to/module
./d.py vo mod download path/to/module
./d.py vo release verify path/to/module
```

Repository layout conventions are documented in:

```text
lang/docs/spec/repository-layout.md
lang/docs/spec/module.md
lang/docs/spec/native-ffi.md
```

## CI System

The local CI task graph is in:

```text
scripts/ci/tasks.toml
scripts/ci/run.py
scripts/ci/plan.py
```

Common groups:

- `smart`: default changed-file based selection.
- `quality`: fmt, clippy, cargo check, WASM check.
- `test`: language tests and release-mode cargo/WASM checks.
- `site`: web/Studio/quickplay build and static smoke checks.
- `release-verify`: first-party module release validation.
- `pr`: broad PR gate.
- `full`: everything.

Run examples:

```sh
./d.py ci smart
./d.py ci pr
./d.py ci task cargo-check
./d.py ci task studio-wasm-build
```

GitHub Actions:

- `.github/workflows/module-system-enforcement.yml`: PR/push task planning and matrix execution.
- `.github/workflows/deploy-site.yml`: builds Studio site, Studio WASM, quickplay assets, and deploys GitHub Pages.
- `.github/workflows/release.yml`: CLI release artifacts and Homebrew update.
- `.github/actions/setup-rust/action.yml`: shared Rust setup.

## Development Workflows

### Change Language Semantics

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
./d.py test gc
cargo check -p vo-web --target wasm32-unknown-unknown
```

Add tests in `lang/test_data/` for language behavior and Rust unit tests near runtime/compiler internals.

### Change Studio Web UI

Start in:

```text
studio/src/App.svelte
studio/src/components
studio/src/lib/services
studio/src/lib/backend
studio/src/stores
```

Typical verification:

```sh
cd studio
npm run build
```

For live development:

```sh
./d.py studio
```

### Change Studio WASM or Compiler Used by Studio

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

Then test in Studio web with a fresh page reload and confirm the loaded build id matches `studio/public/wasm/vo_studio_wasm.build_id`.

### Change Module/Release Logic

Start in:

```text
lang/crates/vo-module
lang/crates/vo-release
lang/docs/spec/module.md
lang/docs/spec/repository-layout.md
scripts/ci/tasks.toml
```

Typical verification:

```sh
./d.py ci task release-verify-vogui
./d.py ci release-verify
```

### Change First-Party Game/UI Libraries

The common sibling repos are:

```text
../vogui
../voplay
../vopack
../vostore
```

Use Volang `vo.work` for local development, but validate release artifacts through `release verify` before publishing or changing locked quickplay packages.

## Debugging Tips

- Use `rg` first for code search.
- Use `./d.py vo dump <file.vob>` to inspect bytecode.
- Use `./d.py vo check <project>` before runtime debugging.
- Use `./d.py studio --build-only` when Studio web appears stale.
- Use browser console logs for Studio web backend and island transport issues.
- Use `lang/docs/dev-notes/` for investigation handoffs; keep issue-specific evidence separate from general onboarding.

## Useful Docs

Read these first:

```text
README.md
studio/docs/getting-started/introduction.md
studio/docs/cli/commands.md
studio/docs/advanced/backends.md
studio/docs/advanced/modules.md
studio/docs/advanced/embedding.md
lang/docs/spec/module.md
lang/docs/spec/repository-layout.md
lang/docs/spec/vm-bytecode.md
lang/docs/dev/d-py-usage.md
```

For architecture history, scan recent files in:

```text
lang/docs/dev-notes/
```

Those notes are design/investigation records, not always current specs. Treat `studio/docs/` and `lang/docs/spec/` as the stronger source for user-facing or canonical behavior.
